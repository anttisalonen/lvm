#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include "stack.h"

// #define DEBUG

#ifdef DEBUG
#define dprintf(...) \
	fprintf(stderr, __VA_ARGS__)
#else
#define dprintf(...)
#endif

enum opcode {
	opcode_add,
	opcode_sub,
	opcode_mul,
	opcode_div,
	opcode_lt,
	opcode_le,
	opcode_eq,
	opcode_dup,
	opcode_drop,
	opcode_nop,
	opcode_swap,
	opcode_new,
	opcode_rstore,
	opcode_rload,
};

enum valuetype {
	valuetype_int,
	valuetype_opcode,
};

typedef struct {
	enum valuetype vt;
	union {
		enum opcode op;
		int32_t intvalue;
	} value;
} stackvalue;

typedef struct {
	int id;
} ref_id;

typedef struct {
	ref_id reference_id;
	size_t mem_size;
	void *allocated_object;
} reference;

#define MAX_NUM_FUNCTIONS 1024

typedef struct {
	int start_addr;
	uint16_t num_params;
} fundata;

static fundata functions[MAX_NUM_FUNCTIONS];

#define MAX_NUM_OBJECTS 1024
static reference objects[MAX_NUM_OBJECTS];

static int next_reference_id = 1;

#define MAX_CALL_DEPTH 1024

static int return_depth = 0;
static struct { 
	int ret_addr;
	int fp;
	int fun_number;
} return_points[MAX_CALL_DEPTH];

#define STACK_SIZE 1024

static int pc;
static stackvalue stack[STACK_SIZE];
static int sp;

#define REF_ID_MASK (0x7 << 28)

static reference *get_obj(ref_id ref_id)
{
	if(ref_id.id <= 0 || ref_id.id >= MAX_NUM_OBJECTS)
		return NULL;
	return &objects[ref_id.id];
}

static ref_id int_to_ref_id(int32_t i)
{
	ref_id id;
	id.id = i & ~(REF_ID_MASK);
	return id;
}

static int is_reference(int32_t i)
{
	if(i < 1)
		return 0;
	if(int_to_ref_id(i).id >= MAX_NUM_OBJECTS)
		return 0;
	return (i & REF_ID_MASK) == REF_ID_MASK;
}

static void print_intvalue(int val, unsigned int indent)
{
	if(indent > 70)
		return;
	int spaces;
	for(spaces = 0; spaces < indent; spaces++)
		fputc(' ', stderr);
	if(is_reference(val)) {
		reference *ref = get_obj(int_to_ref_id(val));
		if(!ref) {
			fprintf(stderr, "REF invalid\n");
		}
		else {
			int i;
			fprintf(stderr, "REF %d of %d at %p\n",
					ref->reference_id.id,
					ref->mem_size,
					ref->allocated_object);
			for(i = 0; i < ref->mem_size / 4; i++) {
				int *value = ref->allocated_object;
				print_intvalue(value[i], indent + 4);
			}
		}
	}
	else {
		fprintf(stderr, "INT %d\n", val);
	}
}

static void print_stackvalue(int sp, const stackvalue *sv)
{
	fprintf(stderr, "[%d] ", sp);
	switch(sv->vt) {
		case valuetype_int:
			print_intvalue(sv->value.intvalue, 0);
			return;
		case valuetype_opcode:
			switch(sv->value.op) {
				case opcode_add:
					fprintf(stderr, "ADD\n");
					return;
				case opcode_sub:
					fprintf(stderr, "SUB\n");
					return;
				case opcode_mul:
					fprintf(stderr, "MUL\n");
					return;
				case opcode_div:
					fprintf(stderr, "DIV\n");
					return;
				case opcode_lt:
					fprintf(stderr, "LT\n");
					return;
				case opcode_le:
					fprintf(stderr, "LE\n");
					return;
				case opcode_eq:
					fprintf(stderr, "EQ\n");
					return;
				case opcode_dup:
					fprintf(stderr, "DUP\n");
					return;
				case opcode_drop:
					fprintf(stderr, "DROP\n");
					return;
				case opcode_nop:
					fprintf(stderr, "NOP\n");
					return;
				case opcode_swap:
					fprintf(stderr, "SWAP\n");
					return;
				case opcode_new:
					fprintf(stderr, "NEW\n");
					return;
				case opcode_rstore:
					fprintf(stderr, "RSTORE\n");
					return;
				case opcode_rload:
					fprintf(stderr, "RLOAD\n");
					return;
			}
		default:
			fprintf(stderr, "<unknown>\n");
			return;
	}
}

static void print_stack(const stackvalue *stack, int sp)
{
	sp--;
	while(sp >= 0) {
		print_stackvalue(sp, &stack[sp]);
		sp--;
	}
}

static void panic(const char *msg)
{
	fprintf(stderr, "%s\n", msg);
	fprintf(stderr, "PC: 0x%04x\n", pc);
	print_stack(stack, sp);
	exit(1);
}

static uint32_t ref_id_to_int(ref_id i)
{
	return i.id | REF_ID_MASK;
}

static int get_int(const char *buf, int *pc, stackvalue *sv)
{
	sv->vt = valuetype_int;
	sv->value.intvalue = (buf[*pc]) & 0x7f;
	sv->value.intvalue += (buf[*pc + 1]) << 7;
	sv->value.intvalue += (buf[*pc + 2]) << 15;
	sv->value.intvalue += (buf[*pc + 3]) << 23;
	if(buf[*pc] & 0x80)
		sv->value.intvalue = -sv->value.intvalue;
	(*pc) += 4;
	return 0;
}

static int get_fun_value(const char *buf, int *pc, unsigned int bufsize)
{
	switch(buf[*pc]) {
		case OPCODE_ADD:
		case OPCODE_SUB:
		case OPCODE_MUL:
		case OPCODE_DIV:
		case OPCODE_LT:
		case OPCODE_LE:
		case OPCODE_EQ:
		case OPCODE_DUP:
		case OPCODE_NEW:
		case OPCODE_DROP:
		case OPCODE_NOP:
		case OPCODE_SWAP:
		case OPCODE_RSTORE:
		case OPCODE_RLOAD:
			(*pc)++;
			return 0;
		case OPCODE_INT:
		case OPCODE_CALLFUN:
		case OPCODE_BRANCH:
		case OPCODE_BRANCHNZ:
		case OPCODE_LOAD:
			(*pc)++;
			if(*pc + 4 >= bufsize)
				return 1;
			(*pc) += 4;
			return 0;
		default:
			return 1;
	}
}

static int stackvalue_is_int(const stackvalue *sv)
{
	return sv->vt == valuetype_int;
}

static enum opcode opcode_to_enum(int opcode)
{
	switch(opcode) {
		case OPCODE_ADD:
			return opcode_add;
		case OPCODE_SUB:
			return opcode_sub;
		case OPCODE_MUL:
			return opcode_mul;
		case OPCODE_DIV:
			return opcode_div;
		case OPCODE_LT:
			return opcode_lt;
		case OPCODE_LE:
			return opcode_le;
		case OPCODE_EQ:
			return opcode_eq;
		case OPCODE_DUP:
			return opcode_dup;
		case OPCODE_NEW:
			return opcode_new;
		case OPCODE_DROP:
			return opcode_drop;
		case OPCODE_SWAP:
			return opcode_swap;
		case OPCODE_RSTORE:
			return opcode_rstore;
		case OPCODE_RLOAD:
			return opcode_rload;
		default: // nop
			return opcode_nop;
	}
}

static int get_value(const char *buf, stackvalue *sv, int *pc, unsigned int bufsize,
		int *interp, stackvalue *stack, int *sp)
{
	*interp = 1;
	switch(buf[*pc]) {
		case OPCODE_ADD:
		case OPCODE_SUB:
		case OPCODE_MUL:
		case OPCODE_DIV:
		case OPCODE_LT:
		case OPCODE_LE:
		case OPCODE_EQ:
		case OPCODE_DUP:
		case OPCODE_NEW:
		case OPCODE_SWAP:
		case OPCODE_DROP:
		case OPCODE_RSTORE:
		case OPCODE_RLOAD:
		case OPCODE_NOP:
			sv->vt = valuetype_opcode;
			sv->value.op = opcode_to_enum(buf[*pc]);
			(*pc)++;
			return 0;
		case OPCODE_INT:
			(*pc)++;
			if(*pc + 4 >= bufsize)
				return 1;
			return get_int(buf, pc, sv);
		case OPCODE_DEFUN_END:
		case OPCODE_RET0:
		case OPCODE_RET1:
			if(return_depth == 0) {
				*pc = -1;
			}
			else {
				if(buf[*pc] == OPCODE_RET0) {
					*sp = return_points[return_depth].fp;
				}
				else if(buf[*pc] == OPCODE_RET1) {
					int old_sp = *sp - 1;
					*sp = return_points[return_depth].fp;
					stack[*sp - 1] = stack[old_sp];
					dprintf("Old sp: %d; sp: %d\n", old_sp, *sp);
				}
				*pc = return_points[return_depth].ret_addr;
				int num_params = functions[return_points[return_depth].fun_number].num_params;
				if(num_params) {
					if(num_params <= *sp) {
						memmove(&stack[*sp - 1 - num_params], &stack[*sp - 1],
								num_params * sizeof(stackvalue));
						*sp -= num_params;
					}
					else {
						dprintf("Function %d has %d parameters - but "
							"only %d elements in stack!\n",
							return_points[return_depth].fun_number,
							num_params, *sp);
					}
				}
				return_depth--;
			}
			*interp = 0;
			return 0;
		case OPCODE_CALLFUN:
			(*pc)++;
			if(*pc + 4 >= bufsize)
				return 1;
			*interp = 0;
			if(get_int(buf, pc, sv))
				return 1;
			if(return_depth + 1 >= MAX_CALL_DEPTH)
				return 1;
			return_depth++;
			return_points[return_depth].ret_addr = *pc;
			return_points[return_depth].fp = *sp;
			return_points[return_depth].fun_number = sv->value.intvalue - 1;
			*pc = functions[sv->value.intvalue - 1].start_addr;
			return 0;
		case OPCODE_BRANCHNZ:
			*interp = 0;
			if(*sp < 1)
				return 1;
			if(!stackvalue_is_int(&stack[*sp - 1]))
				return 1;
			int32_t i1 = stack[*sp - 1].value.intvalue;
			(*sp)--;
			if(!i1) {
				(*pc) += 5;
				return 0;
			}
			/* FALL THROUGH */
		case OPCODE_BRANCH:
			(*pc)++;
			*interp = 0;
			if(*pc + 4 >= bufsize)
				return 1;
			if(get_int(buf, pc, sv))
				return 1;
			(*pc) = sv->value.intvalue;
			return 0;
		case OPCODE_LOAD:
			(*pc)++;
			if(*pc + 4 >= bufsize)
				return 1;
			if(get_int(buf, pc, sv))
				return 1;
			if(sv->value.intvalue < -return_points[return_depth].fp ||
					sv->value.intvalue > *sp - return_points[return_depth].fp + 1)
				return 1;
			*sv = stack[sv->value.intvalue + return_points[return_depth].fp];
			return 0;

		default:
			fprintf(stderr, "Invalid opcode at 0x%0x: 0x%x\n",
					*pc, buf[*pc]);
			return 1;
	}
}

static int interpret_arith_opcode(const stackvalue *sv, stackvalue *stack, int *sp)
{
	int32_t i1;
	int32_t i2;
	if(*sp < 2) {
		fprintf(stderr, "arithmetic with %d elements in stack\n",
				*sp);
		return 1;
	}
	if(!stackvalue_is_int(&stack[*sp - 1])) {
		fprintf(stderr, "arithmetic with no int\n");
		return 1;
	}
	if(!stackvalue_is_int(&stack[*sp - 2])) {
		fprintf(stderr, "arithmetic with no int\n");
		return 1;
	}
	i1 = stack[*sp - 1].value.intvalue;
	i2 = stack[*sp - 2].value.intvalue;
	(*sp)--;
	stack[*sp - 1].vt = valuetype_int;
	switch(sv->value.op) {
		case opcode_add:
			stack[*sp - 1].value.intvalue = i2 + i1;
			dprintf("[%d] %d + %d = %d\n", *sp, i2, i1, stack[*sp - 1].value.intvalue);
			break;
		case opcode_sub:
			stack[*sp - 1].value.intvalue = i2 - i1;
			dprintf("[%d] %d - %d = %d\n", *sp, i2, i1, stack[*sp - 1].value.intvalue);
			break;
		case opcode_mul:
			stack[*sp - 1].value.intvalue = i2 * i1;
			dprintf("[%d] %d * %d = %d\n", *sp, i2, i1, stack[*sp - 1].value.intvalue);
			break;
		case opcode_div:
			stack[*sp - 1].value.intvalue = i2 / i1;
			dprintf("[%d] %d / %d = %d\n", *sp, i2, i1, stack[*sp - 1].value.intvalue);
			break;
		case opcode_lt:
			stack[*sp - 1].value.intvalue = i2 < i1;
			dprintf("[%d] %d < %d = %d\n", *sp, i2, i1, stack[*sp - 1].value.intvalue);
			break;
		case opcode_le:
			stack[*sp - 1].value.intvalue = i2 <= i1;
			dprintf("[%d] %d <= %d = %d\n", *sp, i2, i1, stack[*sp - 1].value.intvalue);
			break;
		case opcode_eq:
			stack[*sp - 1].value.intvalue = i2 == i1;
			dprintf("[%d] %d == %d = %d\n", *sp, i2, i1, stack[*sp - 1].value.intvalue);
			break;
		case opcode_dup:
		case opcode_new:
		case opcode_drop:
		case opcode_nop:
		case opcode_swap:
		case opcode_rstore:
		case opcode_rload:
			break;
	}
	return 0;
}

static int interpret_dup(const stackvalue *sv, stackvalue *stack, int *sp)
{
	if(*sp < 1) {
		fprintf(stderr, "DUP with empty stack\n");
		return 1;
	}
	stack[*sp] = stack[*sp - 1];
#ifdef DEBUG
	if(stackvalue_is_int(&stack[*sp])) {
		dprintf("[%d] DUP %d\n", *sp, stack[*sp].value.intvalue);
	}
#endif
	(*sp)++;
	return 0;
}

static int interpret_drop(const stackvalue *sv, stackvalue *stack, int *sp)
{
	if(*sp < 1) {
		fprintf(stderr, "DROP with empty stack\n");
		return 1;
	}
	(*sp)--;
	return 0;
}

static int interpret_swap(const stackvalue *sv, stackvalue *stack, int *sp)
{
	if(*sp < 2) {
		fprintf(stderr, "SWAP with %d elements\n", *sp);
		return 1;
	}
#ifdef DEBUG
	if(stackvalue_is_int(&stack[*sp - 1]) && stackvalue_is_int(&stack[*sp - 2])) {
		dprintf("[%d] SWAP %d %d\n", *sp,
				stack[*sp - 1].value.intvalue,
				stack[*sp - 2].value.intvalue);
	}
#endif
	stackvalue tmp = stack[*sp - 1];
	stack[*sp - 1] = stack[*sp - 2];
	stack[*sp - 2] = tmp;
	return 0;
}

static stackvalue new_object(int size)
{
	stackvalue tmp;
	uint32_t ref_id = next_reference_id++;
	if(ref_id >= MAX_NUM_OBJECTS) {
		panic("too many objects");
	}
	objects[ref_id].reference_id.id = ref_id;
	objects[ref_id].mem_size = size;
	objects[ref_id].allocated_object = malloc(size);
	if(!objects[ref_id].allocated_object) {
		panic("no memory available");
	}
	tmp.vt = valuetype_int;
	tmp.value.intvalue = ref_id_to_int(objects[ref_id].reference_id);
	dprintf("NEW %d (%p) of size %d\n",
			objects[ref_id].reference_id.id,
			objects[ref_id].allocated_object,
			objects[ref_id].mem_size);
	return tmp;
}

static int interpret_new(const stackvalue *sv, stackvalue *stack, int *sp)
{
	if(*sp < 1) {
		fprintf(stderr, "NEW with empty stack\n");
		return 1;
	}
	stack[*sp - 1] = new_object(stack[*sp - 1].value.intvalue);
	return 0;
}

/*
 * RSTORE: [REF ADDR VAL] => []
 * objs[REF].ADDR = VAL
 */
static int interpret_rstore(const stackvalue *sv, stackvalue *stack, int *sp)
{
	if(*sp < 3) {
		fprintf(stderr, "RSTORE with not enough elements in stack\n");
		return 1;
	}
	if(stack[*sp - 3].vt != valuetype_int || !is_reference(stack[*sp - 3].value.intvalue)) {
		fprintf(stderr, "RSTORE without reference\n");
		return 1;
	}
	reference *ref = get_obj(int_to_ref_id(stack[*sp - 3].value.intvalue));
	if(!ref) {
		fprintf(stderr, "RSTORE with unknown reference\n");
		return 1;
	}
	if(stack[*sp - 2].vt != valuetype_int ||
			stack[*sp - 2].value.intvalue < 0 ||
			stack[*sp - 2].value.intvalue >= ref->mem_size ||
			stack[*sp - 2].value.intvalue % 4 != 0) {
		fprintf(stderr, "RSTORE without valid address\n");
		return 1;
	}
	if(stack[*sp - 1].vt != valuetype_int) {
		fprintf(stderr, "RSTORE without value\n");
		return 1;
	}
	int *value_pointer = ref->allocated_object + stack[*sp - 2].value.intvalue;
	*value_pointer = stack[*sp - 1].value.intvalue;
	*sp -= 3;
	return 0;
}

static int interpret_rload(const stackvalue *sv, stackvalue *stack, int *sp)
{
	if(*sp < 2) {
		fprintf(stderr, "RLOAD with not enough elements in stack\n");
		return 1;
	}
	if(stack[*sp - 3].vt != valuetype_int || !is_reference(stack[*sp - 2].value.intvalue)) {
		fprintf(stderr, "RLOAD without reference\n");
		return 1;
	}
	reference *ref = get_obj(int_to_ref_id(stack[*sp - 2].value.intvalue));
	if(!ref) {
		fprintf(stderr, "RLOAD without valid reference\n");
		return 1;
	}
	if(stack[*sp - 1].vt != valuetype_int ||
			stack[*sp - 1].value.intvalue < 0 ||
			stack[*sp - 1].value.intvalue >= ref->mem_size ||
			stack[*sp - 1].value.intvalue % 4 != 0) {
		fprintf(stderr, "RLOAD without valid address\n");
		return 1;
	}
	int *value_pointer = ref->allocated_object + stack[*sp - 1].value.intvalue;
	*sp -= 2;
	stack[*sp].vt = valuetype_int;
	stack[*sp].value.intvalue = *value_pointer;
	(*sp)++;
	return 0;
}

static int interpret(const stackvalue *sv, stackvalue *stack, int *sp)
{
	switch(sv->vt) {
		case valuetype_int:
			stack[*sp] = *sv;
			(*sp)++;
			return 0;
		case valuetype_opcode:
			switch(sv->value.op) {
				case opcode_add:
				case opcode_sub:
				case opcode_mul:
				case opcode_div:
				case opcode_lt:
				case opcode_le:
				case opcode_eq:
					return interpret_arith_opcode(sv, stack, sp);
				case opcode_dup:
					return interpret_dup(sv, stack, sp);
				case opcode_drop:
					return interpret_drop(sv, stack, sp);
				case opcode_nop:
					return 0;
				case opcode_swap:
					return interpret_swap(sv, stack, sp);
				case opcode_new:
					return interpret_new(sv, stack, sp);
				case opcode_rstore:
					return interpret_rstore(sv, stack, sp);
				case opcode_rload:
					return interpret_rload(sv, stack, sp);
			}
		default:
			return 1;
	}
}

static int run_code(const char *buf, unsigned int bufsize)
{
	pc = functions[0].start_addr;
	sp = 0;
	return_points[0].fp = 0;
	while(pc != bufsize && pc != -1) {
		stackvalue sv;
		int interp;
		if(sp >= STACK_SIZE) {
			panic("Stack overflow");
			return 1;
		}
		if(get_value(buf, &sv, &pc, bufsize, &interp, stack, &sp)) {
			panic("Invalid instruction");
			return 1;
		}
		if(interp) {
			if(interpret(&sv, stack, &sp)) {
				panic("Invalid instruction on interpret");
				return 1;
			}
		}
	}
	print_stack(stack, sp);
	return 0;
}

static void get_unsigned_shorts(const char *buf, int *pc,
		uint16_t *a, uint16_t *b)
{
	*a  = (buf[*pc]) & 0x7f;
	*a += (buf[*pc + 1]) << 7;
	*b  = (buf[*pc + 2]) & 0x7f;
	*b += (buf[*pc + 3]) << 7;
	if(buf[*pc] & 0x80)
		*a = -*a;
	if(buf[*(pc + 2)] & 0x80)
		*b = -*b;
	(*pc) += 4;
	return;
}

static int get_fundef(const char *buf, int *pc, unsigned int bufsize,
		int *current_fundef)
{
	uint16_t fun_number;
	uint16_t fun_params;
	switch(buf[*pc]) {
		case OPCODE_DEFUN_START:
			if(*current_fundef) {
				fprintf(stderr, "Function definition within a function definition\n");
				return 1;
			}
			(*pc)++;
			if(*pc + 4 >= bufsize)
				return 1;
			get_unsigned_shorts(buf, pc, &fun_params, &fun_number);
			if(fun_number >= MAX_NUM_FUNCTIONS)
				return 1;
			if(fun_number <= 0)
				return 1;
			if(functions[fun_number - 1].start_addr != 0)
				return 1;
			functions[fun_number - 1].start_addr = *pc;
			functions[fun_number - 1].num_params = fun_params;
			*current_fundef = fun_number;
			return 0;
		case OPCODE_DEFUN_END:
		case OPCODE_RET0:
		case OPCODE_RET1:
			if(!*current_fundef)
				return 1;
			*current_fundef = 0;
			(*pc)++;
			return 0;
		default:
			fprintf(stderr, "Unexpected opcode at 0x%x: 0x%x\n",
					*pc, buf[*pc]);
			return 1;
	}
}

static int setup_functions(const char *buf, int bufsize)
{
	int pc = 0;
	int current_fundef = 0;
	while(pc != bufsize) {
		if(get_fun_value(buf, &pc, bufsize)) {
			if(get_fundef(buf, &pc, bufsize, &current_fundef)) {
				fprintf(stderr, "Invalid function definition (function %d)\n",
						current_fundef);
				return 1;
			}
		}
	}
	return 0;
}

int parse_buffer(const char *buf, int bufsize)
{
	if(setup_functions(buf, bufsize))
		return 1;
	return run_code(buf, bufsize);
}

int parse_file(const char *filename)
{
	FILE *fp;
	if(!strcmp("-", filename))
		fp = stdin;
	else
		fp = fopen(filename, "rb");
	if(!fp) {
		perror("fopen");
		return 1;
	}
	char buf[1024];
	size_t ret = fread(buf, 1, 1024, fp);
	if(ferror(fp)) {
		perror("read");
		fclose(fp);
		return 1;
	}
	fclose(fp);
	return parse_buffer(buf, ret);
}


