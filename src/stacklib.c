#include <stdio.h>
#include <stdint.h>
#include <string.h>

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

#define MAX_NUM_FUNCTIONS 1024

static int functions[MAX_NUM_FUNCTIONS];

#define MAX_CALL_DEPTH 1024

static int return_depth = 0;
static struct { 
	int ret_addr;
	int fp;
} return_points[MAX_CALL_DEPTH];

static int get_int(const char *buf, int *pc, stackvalue *sv)
{
	sv->vt = valuetype_int;
	sv->value.intvalue = (buf[*pc]) & 0x7f;
	sv->value.intvalue += (buf[*pc + 1]) << 8;
	sv->value.intvalue += (buf[*pc + 2]) << 16;
	sv->value.intvalue += (buf[*pc + 3]) << 24;
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
		case OPCODE_DROP:
		case OPCODE_NOP:
		case OPCODE_SWAP:
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
		case OPCODE_DROP:
			return opcode_drop;
		case OPCODE_SWAP:
			return opcode_swap;
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
		case OPCODE_SWAP:
		case OPCODE_DROP:
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
			if(return_depth == 0) {
				(*pc) = -1;
			}
			else {
				(*pc) = return_points[return_depth--].ret_addr;
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
			*pc = functions[sv->value.intvalue - 1];
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
			sv->value.intvalue = stack[sv->value.intvalue + return_points[return_depth].fp].value.intvalue;
			printf("Load %d; fp: %d; sp: %d\n", sv->value.intvalue, return_points[return_depth].fp, *sp);
			return 0;

	return 0;
		default:
			fprintf(stderr, "Invalid opcode at 0x%0x: 0x%x\n",
					*pc, buf[*pc]);
			return 1;
	}
}

static void print_stackvalue(const stackvalue *sv)
{
	switch(sv->vt) {
		case valuetype_int:
			printf("INT %d\n", sv->value.intvalue);
			return;
		case valuetype_opcode:
			switch(sv->value.op) {
				case opcode_add:
					printf("ADD\n");
					return;
				case opcode_sub:
					printf("SUB\n");
					return;
				case opcode_mul:
					printf("MUL\n");
					return;
				case opcode_div:
					printf("DIV\n");
					return;
				case opcode_lt:
					printf("LT\n");
					return;
				case opcode_le:
					printf("LE\n");
					return;
				case opcode_eq:
					printf("EQ\n");
					return;
				case opcode_dup:
					printf("DUP\n");
					return;
				case opcode_drop:
					printf("DROP\n");
					return;
				case opcode_nop:
					printf("NOP\n");
					return;
				case opcode_swap:
					printf("SWAP\n");
					return;
			}
		default:
			printf("<unknown>\n");
			return;
	}
}

static void print_stack(const stackvalue *stack, int sp)
{
	sp--;
	while(sp >= 0) {
		print_stackvalue(&stack[sp]);
		sp--;
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
		case opcode_drop:
		case opcode_nop:
		case opcode_swap:
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
			}
		default:
			return 1;
	}
}

#define STACK_SIZE 1024

static int run_code(const char *buf, unsigned int bufsize)
{
	int pc = functions[0];
	return_points[0].fp = 0;
	stackvalue stack[STACK_SIZE];
	int sp = 0;
	while(pc != bufsize && pc != -1) {
		stackvalue sv;
		int interp;
		if(sp >= STACK_SIZE) {
			fprintf(stderr, "Stack overflow\n");
			return 1;
		}
		if(get_value(buf, &sv, &pc, bufsize, &interp, stack, &sp)) {
			fprintf(stderr, "Invalid instruction on value\n");
			return 1;
		}
		if(interp) {
			if(interpret(&sv, stack, &sp)) {
				fprintf(stderr, "Invalid instruction on interpret\n");
				return 1;
			}
		}
	}
	print_stack(stack, sp);
	return 0;
}

static int get_fundef(const char *buf, int *pc, unsigned int bufsize,
		int *current_fundef)
{
	stackvalue sv;
	switch(buf[*pc]) {
		case OPCODE_DEFUN_START:
			if(*current_fundef) {
				fprintf(stderr, "Function definition within a function definition\n");
				return 1;
			}
			(*pc)++;
			if(*pc + 4 >= bufsize)
				return 1;
			if(get_int(buf, pc, &sv))
				return 1;
			if(sv.value.intvalue >= MAX_NUM_FUNCTIONS)
				return 1;
			if(sv.value.intvalue <= 0)
				return 1;
			if(functions[sv.value.intvalue - 1] != 0)
				return 1;
			functions[sv.value.intvalue - 1] = *pc;
			*current_fundef = sv.value.intvalue;
			return 0;
		case OPCODE_DEFUN_END:
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


