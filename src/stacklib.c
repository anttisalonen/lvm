#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <dlfcn.h>

#include <ffi.h>

#include "stack.h"

/* #define DEBUG */

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
	valuetype_fun_id,
	valuetype_opcode,
	valuetype_object,
	valuetype_closure,
};

typedef struct {
	enum valuetype vt;
	union {
		enum opcode op;
		int32_t intvalue;
	} value;
} stackvalue;

typedef struct {
	int reference_id;
	size_t mem_size;
	void *allocated_object;
	int closure;
} reference;

#define MAX_NUM_FUNCTIONS 1024

enum ffitype {
	ffitype_void,
	ffitype_uint8,
	ffitype_sint8,
	ffitype_uint16,
	ffitype_sint16,
	ffitype_uint32,
	ffitype_sint32,
	ffitype_uint64,
	ffitype_sint64,
	ffitype_float,
	ffitype_double,
	ffitype_uchar,
	ffitype_schar,
	ffitype_ushort,
	ffitype_sshort,
	ffitype_uint,
	ffitype_sint,
	ffitype_ulong,
	ffitype_slong,
	ffitype_longdouble,
	ffitype_pointer,
};

typedef struct {
	int ffi;
	uint16_t num_params;
	union {
		struct {
			int start_addr;
		} native;
		struct {
			void *ffiptr;
			enum ffitype rettype;
			enum ffitype params[MAX_NUM_FFI_PARAMETERS];
			ffi_type *args[MAX_NUM_FFI_PARAMETERS];
			ffi_cif cif;
		} ffi;
	} call;
} fundata;

static int fun_in_use(const fundata *f)
{
	return f->ffi || f->call.native.start_addr;
}

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
	int cls_ret_sp;
	int cls_sp_pos;
} return_points[MAX_CALL_DEPTH];

#define STACK_SIZE 1024

struct closure {
	int pc;
	int sp;
	int stack_size;
	int num_params;
	/* NOTE: stack must be the last element in the struct
	 * (will be treated as an array) */
	stackvalue stack;
};

static int pc;
static stackvalue stack[STACK_SIZE];
static int sp;

#define REF_ID_MASK (0xf << 28)
#define CLS_ID_MASK (0x1 << 28)
#define OBJ_ID_MASK (0x2 << 28)

static reference *get_obj(int ref_id)
{
	if(!(ref_id & REF_ID_MASK))
		return NULL;
	ref_id &= ~REF_ID_MASK;
	if(ref_id <= 0 || ref_id >= MAX_NUM_OBJECTS)
		return NULL;
	return &objects[ref_id];
}

static void print_stackvalue(const stackvalue *sv, int sp,
		unsigned int indent)
{
	reference *ref;
	if(indent > 70)
		return;
	int spaces;
	if(indent == 0)
		fprintf(stderr, "[%d] ", sp);
	for(spaces = 0; spaces < indent; spaces++)
		fputc(' ', stderr);
	switch(sv->vt) {
		case valuetype_int:
			fprintf(stderr, "INT %d\n", sv->value.intvalue);
			break;
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
			break;
		case valuetype_fun_id:
			fprintf(stderr, "FUN_ID %d\n", sv->value.intvalue);
			break;
		case valuetype_object:
			ref = get_obj(sv->value.intvalue);
			if(!ref) {
				fprintf(stderr, "REF invalid\n");
			}
			else {
				int i;
				fprintf(stderr, "REF %d of %zd at %p\n",
						ref->reference_id,
						ref->mem_size,
						ref->allocated_object);
				for(i = 0; i < 4 && i < ref->mem_size / 4; i++) {
					int *value = ref->allocated_object;
					stackvalue sv2;
					sv2.vt = valuetype_int;
					sv2.value.intvalue = value[i];
					if((sv2.value.intvalue & OBJ_ID_MASK) == OBJ_ID_MASK)
						sv2.vt = valuetype_object;
					else if((sv2.value.intvalue & CLS_ID_MASK) == CLS_ID_MASK)
						sv2.vt = valuetype_closure;
					print_stackvalue(&sv2, sp, indent + 4);
				}
			}
			break;
		case valuetype_closure:
			fprintf(stderr, "CLS %d\n",
					sv->value.intvalue & ~REF_ID_MASK);
			break;
	}
}

static void print_stack(const stackvalue *stack, int sp)
{
	sp--;
	while(sp >= 0) {
		print_stackvalue(&stack[sp], sp, 0);
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

static int get_num(const char *buf, int *pc, stackvalue *sv,
		enum valuetype vt)
{
	sv->vt = vt;
	sv->value.intvalue = (buf[*pc]) & 0x7f;
	sv->value.intvalue += (buf[*pc + 1]) << 7;
	sv->value.intvalue += (buf[*pc + 2]) << 15;
	sv->value.intvalue += (buf[*pc + 3]) << 23;
	if(buf[*pc] & 0x80)
		sv->value.intvalue = -sv->value.intvalue;
	(*pc) += 4;
	return 0;
}

static int get_fun_id(const char *buf, int *pc, stackvalue *sv)
{
	return get_num(buf, pc, sv, valuetype_fun_id);
}

static int get_int(const char *buf, int *pc, stackvalue *sv)
{
	return get_num(buf, pc, sv, valuetype_int);
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
		case OPCODE_CALLPFUN:
		case OPCODE_END_THUNK:
			(*pc)++;
			return 0;
		case OPCODE_INT:
		case OPCODE_PFUN_ID:
		case OPCODE_CALLFUN:
		case OPCODE_BRANCH:
		case OPCODE_BRANCHNZ:
		case OPCODE_START_THUNK:
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

static int stackvalue_is_fun_id(const stackvalue *sv)
{
	return sv->vt == valuetype_fun_id;
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
		default: /* nop */
			return opcode_nop;
	}
}

static stackvalue new_reference(int size, enum valuetype vt)
{
	stackvalue tmp;
	uint32_t ref_id = next_reference_id++;
	if(ref_id >= MAX_NUM_OBJECTS) {
		panic("too many objects");
	}
	objects[ref_id].reference_id = ref_id;
	objects[ref_id].mem_size = size;
	objects[ref_id].closure = vt == valuetype_closure;
	objects[ref_id].allocated_object = malloc(size);
	if(!objects[ref_id].allocated_object) {
		panic("no memory available");
	}
	tmp.vt = vt;
	tmp.value.intvalue = ref_id | (vt == valuetype_closure ? CLS_ID_MASK : OBJ_ID_MASK);
	dprintf("NEW %d (%p) of size %d\n",
			objects[ref_id].reference_id,
			objects[ref_id].allocated_object,
			objects[ref_id].mem_size);
	return tmp;
}

static int open_closure(int clsdepth, stackvalue *stack, int *sp, int *pc,
		int curr_opcode_length)
{
	stackvalue *sv = &stack[*sp - clsdepth];
	const reference *ref = get_obj(sv->value.intvalue);
	struct closure *cls;
	if(!ref)
		return 1;
	cls = ref->allocated_object;
	memcpy(&stack[*sp], &cls->stack, cls->stack_size * sizeof(stackvalue));
	return_depth++;
	return_points[return_depth].ret_addr = *pc - curr_opcode_length;
	return_points[return_depth].fp = *sp + cls->num_params;
	return_points[return_depth].fun_number = -1;
	return_points[return_depth].cls_ret_sp = *sp;
	return_points[return_depth].cls_sp_pos = *sp - clsdepth;
	*pc = cls->pc;
	*sp += cls->stack_size;
	return 0;
}

static int32_t convert_from_ffi(int *rc, enum ffitype t)
{
	switch(t) {
		case ffitype_uint8:
			return *(uint8_t*)rc;
		case ffitype_sint8:
			return *(int8_t*)rc;
		case ffitype_uint16:
			return *(uint16_t*)rc;
		case ffitype_sint16:
			return *(int16_t*)rc;
		case ffitype_uint32:
			return *(uint32_t*)rc;
		case ffitype_sint32:
			return *(int32_t*)rc;
		case ffitype_uint64:
			return *(uint64_t*)rc;
		case ffitype_sint64:
			return *(int64_t*)rc;
		case ffitype_float:
			return *(float*)rc;
		case ffitype_double:
			return *(double*)rc;
		case ffitype_uchar:
			return *(unsigned char*)rc;
		case ffitype_schar:
			return *(signed char*)rc;
		case ffitype_ushort:
			return *(unsigned short*)rc;
		case ffitype_sshort:
			return *(signed short*)rc;
		case ffitype_uint:
			return *(unsigned int*)rc;
		case ffitype_sint:
			return *(signed int*)rc;
		case ffitype_ulong:
			return *(unsigned long*)rc;
		case ffitype_slong:
			return *(signed long*)rc;
		case ffitype_longdouble:
			return *(long double*)rc;
		case ffitype_pointer:
			fprintf(stderr, "Unable to convert FFI pointers\n");
			abort();
			return 0;
		case ffitype_void:
		default:
			fprintf(stderr, "Unable to convert FFI void value\n");
			return 0;
	}
}

static void convert_to_ffi(void **value, int32_t intvalue,
		enum ffitype ffitype)
{
	switch(ffitype) {
		case ffitype_uint8:
			*(uint8_t*)value = (uint8_t)intvalue;
			break;
		case ffitype_sint8:
			*(int8_t*)value = (int8_t)intvalue;
			break;
		case ffitype_uint16:
			*(uint16_t*)value = (uint16_t)intvalue;
			break;
		case ffitype_sint16:
			*(int16_t*)value = (int16_t)intvalue;
			break;
		case ffitype_uint32:
			*(uint32_t*)value = (uint32_t)intvalue;
			break;
		case ffitype_sint32:
			**(int32_t**)value = (int32_t)intvalue;
			break;
		case ffitype_uint64:
			*(uint64_t*)value = (uint64_t)intvalue;
			break;
		case ffitype_sint64:
			*(int64_t*)value = (int64_t)intvalue;
			break;
		case ffitype_float:
			*(float*)value = (float)intvalue;
			break;
		case ffitype_double:
			*(double*)value = (double)intvalue;
			break;
		case ffitype_uchar:
			*(unsigned char*)value = (unsigned char)intvalue;
			break;
		case ffitype_schar:
			*(signed char*)value = (signed char)intvalue;
			break;
		case ffitype_ushort:
			*(unsigned short*)value = (unsigned short)intvalue;
			break;
		case ffitype_sshort:
			*(signed short*)value = (signed short)intvalue;
			break;
		case ffitype_uint:
			*(unsigned int*)value = (unsigned int)intvalue;
			break;
		case ffitype_sint:
			*(signed int*)value = (signed int)intvalue;
			break;
		case ffitype_ulong:
			*(unsigned long*)value = (unsigned long)intvalue;
			break;
		case ffitype_slong:
			*(signed long*)value = (signed long)intvalue;
			break;
		case ffitype_longdouble:
			*(long double*)value = (long double)intvalue;
			break;
		case ffitype_pointer:
			fprintf(stderr, "Unable to convert FFI pointers\n");
			abort();
			break;
		case ffitype_void:
		default:
			fprintf(stderr, "Unable to convert FFI void value\n");
			break;
	}
}

static int get_value(const char *buf, stackvalue *sv, int *pc, unsigned int bufsize,
		int *interp, stackvalue *stack, int *sp)
{
	struct closure *cls;
	int stack_size, stack_size_bytes;
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
		case OPCODE_PFUN_ID:
			(*pc)++;
			if(*pc + 4 >= bufsize)
				return 1;
			return buf[*pc - 1] == OPCODE_INT ? get_int(buf, pc, sv)
				: get_fun_id(buf, pc, sv);
		case OPCODE_DEFUN_END:
		case OPCODE_RET0:
		case OPCODE_RET1:
			if(return_depth == 0) {
				/* pc == -1 terminates */
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
				/* TODO: move this check to function address buildup */
				if(return_points[return_depth].fun_number == -1) {
					fprintf(stderr, "Function end within closure definition\n");
					return 1;
				}
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
		case OPCODE_CALLPFUN:
			(*pc)++;
			*interp = 0;
			if(buf[*pc - 1] == OPCODE_CALLFUN) {
				if(*pc + 4 >= bufsize)
					return 1;
				if(get_int(buf, pc, sv))
					return 1;
			}
			else { /* OPCODE_CALLPFUN */
				if(*sp < 1)
					return 1;
				if(!stackvalue_is_fun_id(&stack[*sp - 1]))
					return 1;
				if(return_depth + 1 >= MAX_CALL_DEPTH)
					return 1;
				*sv = stack[*sp - 1];
				(*sp)--;
			}
			if(!functions[sv->value.intvalue - 1].ffi) {
				return_depth++;
				return_points[return_depth].ret_addr = *pc;
				return_points[return_depth].fp = *sp;
				return_points[return_depth].fun_number = sv->value.intvalue - 1;
				*pc = functions[sv->value.intvalue - 1].call.native.start_addr;
			}
			else {
				/* FFI call */
				int rc[16]; /* large enough */
				void *values[MAX_NUM_FFI_PARAMETERS];
				int i = 0;
				enum ffitype paramtype;
				fundata *fun = &functions[sv->value.intvalue - 1];
				paramtype = fun->call.ffi.params[0];
				for(i = 0; i < fun->num_params; i++) {
					if(*sp < 1)
						return 1;
					if(!stackvalue_is_int(&stack[*sp - 1]))
						return 1;
					convert_to_ffi(&values[i], stack[*sp - 1].value.intvalue,
							paramtype);
					(*sp)--;
					i++;
					paramtype = fun->call.ffi.params[i];
				}
				ffi_call(&fun->call.ffi.cif, fun->call.ffi.ffiptr,
						&rc, values);
				if(fun->call.ffi.rettype != ffitype_void) {
					int32_t i1 = convert_from_ffi(rc,
							fun->call.ffi.rettype);
					stack[*sp].vt = valuetype_int;
					stack[*sp].value.intvalue = i1;
					(*sp)++;
				}
			}
			return 0;
		case OPCODE_BRANCHNZ:
			*interp = 0;
			if(*sp < 1)
				return 1;
			if(stack[*sp - 1].vt == valuetype_closure)
				return open_closure(1, stack, sp, pc, 0);
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

		case OPCODE_START_THUNK:
			stack_size = *sp - (return_depth == 0 ? 0 : return_points[return_depth - 1].fp);
			stack_size_bytes = stack_size * sizeof(stackvalue);
			*interp = 0;
			(*pc)++;
			if(*pc + 4 >= bufsize)
				return 1;
			if(get_int(buf, pc, sv))
				return 1;

			stack[*sp] = new_reference(sizeof(struct closure) + stack_size_bytes,
					valuetype_closure);
			reference *ref = get_obj(stack[*sp].value.intvalue);
			if(!ref)
				panic("No reference on thunk");
			cls = ref->allocated_object;
			cls->pc = *pc;
			cls->stack_size = stack_size;
			cls->num_params = return_depth == 0 ? 0 :
				return_points[return_depth].fp - return_points[return_depth - 1].fp;
			memcpy(&cls->stack, &stack[*sp - stack_size], stack_size_bytes);

			(*pc) = sv->value.intvalue;
			(*sp)++;
			return 0;

		case OPCODE_END_THUNK:
			/* TODO: move this check to function address buildup */
			if(return_points[return_depth].fun_number != -1) {
				fprintf(stderr, "Closure end within function\n");
				return 1;
			}
			/* On closure end only the topmost value gets copied */
			int old_sp = *sp - 1;
			*sp = return_points[return_depth].cls_ret_sp;
			stack[return_points[return_depth].cls_sp_pos] = stack[old_sp];
			*pc = return_points[return_depth].ret_addr;
			return_depth--;
			*interp = 0;
			return 0;

		default:
			fprintf(stderr, "Invalid opcode at 0x%0x: 0x%x\n",
					*pc, buf[*pc]);
			return 1;
	}
}

static int interpret_arith_opcode(const stackvalue *sv, stackvalue *stack, int *sp,
		int *pc)
{
	int32_t i1;
	int32_t i2;
	if(*sp < 2) {
		fprintf(stderr, "arithmetic with %d elements in stack\n",
				*sp);
		return 1;
	}
	if(stack[*sp - 1].vt == valuetype_closure)
		return open_closure(1, stack, sp, pc, 1);
	if(stack[*sp - 2].vt == valuetype_closure)
		return open_closure(2, stack, sp, pc, 1);
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

static int interpret_new(const stackvalue *sv, stackvalue *stack, int *sp)
{
	if(*sp < 1) {
		fprintf(stderr, "NEW with empty stack\n");
		return 1;
	}
	stack[*sp - 1] = new_reference(stack[*sp - 1].value.intvalue, valuetype_object);
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
	if(stack[*sp - 3].vt != valuetype_object) {
		fprintf(stderr, "RSTORE without object\n");
		return 1;
	}
	reference *ref = get_obj(stack[*sp - 3].value.intvalue);
	if(!ref) {
		fprintf(stderr, "RSTORE with unknown object\n");
		return 1;
	}
	if(stack[*sp - 2].vt != valuetype_int ||
			stack[*sp - 2].value.intvalue < 0 ||
			stack[*sp - 2].value.intvalue >= ref->mem_size ||
			stack[*sp - 2].value.intvalue % 4 != 0) {
		fprintf(stderr, "RSTORE without valid address\n");
		return 1;
	}
	int *value_pointer = ref->allocated_object + stack[*sp - 2].value.intvalue;
	*value_pointer = stack[*sp - 1].value.intvalue;
	if(stack[*sp - 1].vt == valuetype_object)
		*value_pointer |= OBJ_ID_MASK;
	else if(stack[*sp - 1].vt == valuetype_closure)
		*value_pointer |= CLS_ID_MASK;
	*sp -= 3;
	return 0;
}

static int interpret_rload(const stackvalue *sv, stackvalue *stack, int *sp)
{
	if(*sp < 2) {
		fprintf(stderr, "RLOAD with not enough elements in stack\n");
		return 1;
	}
	if(stack[*sp - 2].vt != valuetype_object) {
		fprintf(stderr, "RLOAD without object (value type %d with value %d)\n",
				stack[*sp - 2].vt, stack[*sp - 2].value.intvalue);
		return 1;
	}
	reference *ref = get_obj(stack[*sp - 2].value.intvalue);
	if(!ref) {
		fprintf(stderr, "RLOAD without valid object\n");
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
	if((*value_pointer & OBJ_ID_MASK) == OBJ_ID_MASK)
		stack[*sp].vt = valuetype_object;
	else if((*value_pointer & CLS_ID_MASK) == CLS_ID_MASK)
		stack[*sp].vt = valuetype_closure;
	(*sp)++;
	return 0;
}

static int interpret(const stackvalue *sv, stackvalue *stack, int *sp, int *pc)
{
	switch(sv->vt) {
		case valuetype_int:
		case valuetype_fun_id:
		case valuetype_object:
		case valuetype_closure:
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
					return interpret_arith_opcode(sv, stack, sp, pc);
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
	pc = functions[0].call.native.start_addr;
	sp = 0;
	return_points[0].fp = 0;
	while((pc != bufsize && pc != -1) || (sp > 0 &&
			stack[sp - 1].vt == valuetype_closure &&
			 !open_closure(1, stack, &sp, &pc, 0) && pc != bufsize && pc != -1)) {
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
			if(interpret(&sv, stack, &sp, &pc)) {
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
	if(buf[*pc + 2] & 0x80)
		*b = -*b;
	*pc += 4;
	return;
}

static ffi_type *ffi_enum_to_ffitype(enum ffitype t)
{
	switch(t) {
		case ffitype_uint8:
			return &ffi_type_uint8;
		case ffitype_sint8:
			return &ffi_type_sint8;
		case ffitype_uint16:
			return &ffi_type_uint16;
		case ffitype_sint16:
			return &ffi_type_sint16;
		case ffitype_uint32:
			return &ffi_type_uint32;
		case ffitype_sint32:
			return &ffi_type_sint32;
		case ffitype_uint64:
			return &ffi_type_uint64;
		case ffitype_sint64:
			return &ffi_type_sint64;
		case ffitype_float:
			return &ffi_type_float;
		case ffitype_double:
			return &ffi_type_double;
		case ffitype_uchar:
			return &ffi_type_uchar;
		case ffitype_schar:
			return &ffi_type_schar;
		case ffitype_ushort:
			return &ffi_type_ushort;
		case ffitype_sshort:
			return &ffi_type_sshort;
		case ffitype_uint:
			return &ffi_type_uint;
		case ffitype_sint:
			return &ffi_type_sint;
		case ffitype_ulong:
			return &ffi_type_ulong;
		case ffitype_slong:
			return &ffi_type_slong;
		case ffitype_longdouble:
			return &ffi_type_longdouble;
		case ffitype_pointer:
			return &ffi_type_pointer;
		case ffitype_void:
			return &ffi_type_void;
		default:
			fprintf(stderr, "Unknown FFI type\n");
			return NULL;
	}
}

static int prep_ffi_cif(int fun_number)
{
	int i;
	for(i = 0; i < functions[fun_number - 1].num_params; i++) {
		functions[fun_number - 1].call.ffi.args[i] =
			ffi_enum_to_ffitype(functions[fun_number - 1].call.ffi.params[i]);
	}
	return ffi_prep_cif(&functions[fun_number - 1].call.ffi.cif,
			FFI_DEFAULT_ABI, 1,
			ffi_enum_to_ffitype(functions[fun_number - 1].call.ffi.rettype),
			functions[fun_number - 1].call.ffi.args) == FFI_OK;
}

struct libdata {
	void *dlhandle;
	char libname[MAX_LIBNAME_LENGTH];
};

static struct libdata libraries[MAX_NUM_LIBS];

static int add_ffidef(const char *buf, int *pc, int bufsize)
{
	enum ffitype ffi_rettype;
	uint16_t ffi_ret;
	uint16_t fun_number;
	int more_params = 1;
	ffi_type *ffitype;
	int num_params = 0;
	char ffiname[128];
	int i;
	void *ffiptr = NULL;
	char *dlerr;

	if(*pc + 4 >= bufsize)
		return 1;

	get_unsigned_shorts(buf, pc, &fun_number, &ffi_ret);
	ffi_rettype = ffi_ret;
	ffitype = ffi_enum_to_ffitype(ffi_rettype);
	if(!ffitype) {
		fprintf(stderr, "FFI return type 0x%02x is invalid\n",
				ffi_rettype);
		return 1;
	}
	else {
		functions[fun_number - 1].call.ffi.rettype = ffi_rettype;
	}

	if(fun_number >= MAX_NUM_FUNCTIONS)
		return 1;
	if(fun_number <= 0)
		return 1;

	if(fun_in_use(&functions[fun_number - 1])) {
		fprintf(stderr, "FFI function %d in use\n",
				fun_number);
		return 1;
	}

	/* parameters */
	memset(functions[fun_number - 1].call.ffi.params,
			0x00, sizeof(functions[fun_number - 1].call.ffi.params));

	while(more_params) {
		for(i = 0; i < 4 && *pc < bufsize; i++, (*pc)++) {
			ffitype = ffi_enum_to_ffitype(buf[*pc]);
			if(!ffitype) {
				fprintf(stderr, "FFI parameter 0x%02x is invalid\n",
						buf[*pc]);
				return 1;
			}
			if(more_params) {
				functions[fun_number - 1].call.ffi.params[num_params] = buf[*pc];
				num_params++;
			}
			if(functions[fun_number - 1].call.ffi.params[num_params] == ffitype_void)
				more_params = 0;
		}
		if(num_params + 4 < MAX_NUM_FFI_PARAMETERS)
			more_params = 0;
	}

	/* function name */
	memset(ffiname, 0x00, sizeof(ffiname));
	i = 0;
	while(i < sizeof(ffiname) - 1 && *pc < bufsize && buf[*pc]) {
		ffiname[i] = buf[*pc];
		i++;
		(*pc)++;
	}
	if(i == sizeof(ffiname) - 1) {
		fprintf(stderr, "Too long FFI name (max length: %zd characters)\n",
				sizeof(ffiname - 1));
		return 1;
	}

	for(i = 0; i < MAX_NUM_LIBS; i++) {
		if(libraries[i].dlhandle) {
			dlerror();
			ffiptr = dlsym(libraries[i].dlhandle, ffiname);
			dlerr = dlerror();
			if(ffiptr && !dlerr)
				break;
		}
	}
	if(!ffiptr) {
		fprintf(stderr, "Could not find function '%s'.\n",
				ffiname);
		return 1;
	}
	functions[fun_number - 1].ffi = 1;
	functions[fun_number - 1].call.ffi.ffiptr = ffiptr;
	functions[fun_number - 1].num_params = num_params;
	if(!prep_ffi_cif(fun_number)) {
		fprintf(stderr, "ffi_prep_cif failed\n");
		return 1;
	}

	return 0;
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
			if(fun_in_use(&functions[fun_number - 1]))
				return 1;
			functions[fun_number - 1].ffi = 0;
			functions[fun_number - 1].call.native.start_addr = *pc;
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
		case OPCODE_FFIDEF:
			if(*current_fundef) {
				fprintf(stderr, "FFI definition within a function definition\n");
				return 1;
			}
			(*pc)++;
			return add_ffidef(buf, pc, bufsize);
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

static int get_library_names(const char *buf, int *len)
{
	int i = 0, j = 0;
	const char *ptr = buf;
	*len = 0;
	while(i < MAX_NUM_LIBS) {
		if(*ptr == '\0') {
			/* done */
			(*len)++;
			return 1;
		}
		(*len)++;
		if(*ptr == ' ') {
			/* next lib */
			ptr++;
			if(j == 0) {
				return 0;
			}
			else {
				j = 0;
				i++;
			}
		}
		else {
			/* copy character */
			libraries[i].libname[j] = *ptr;
			ptr++;
			j++;
			if(j >= MAX_LIBNAME_LENGTH)
				return 0;
		}
	}
	return 0;
}

static void make_lib_filename(const char *libname, char *buf, size_t n)
{
	snprintf(buf, n - 1, "lib%s.so",
			libname);
	buf[n - 1] = '\0';
}

int parse_buffer(const char *buf, int bufsize)
{
	int ret;

	int correct_magic = bufsize > 4;
	int vers;
	int lib_length = 0;
	int lib_index = 0;

	if(!correct_magic) {
		fprintf(stderr, "Not a valid LVM bytecode file.\n");
		return 1;
	}

	correct_magic = buf[0] == MAGIC_00 &&
		buf[1] == MAGIC_01 &&
		buf[2] == MAGIC_02 &&
		buf[3] == MAGIC_03;
	if(!correct_magic) {
		fprintf(stderr, "Not a valid LVM bytecode file.\n");
		return 1;
	}

	vers = buf[4];
	if(vers != CURRENT_VERSION) {
		fprintf(stderr, "Bytecode has version %d. Only "
			       "version %d is supported.\n",
			       vers, CURRENT_VERSION);
		return 1;
	}

	if(!get_library_names(&buf[5], &lib_length)) {
		fprintf(stderr, "Invalid library configuration.\n");
		return 1;
	}

	while(lib_index < MAX_NUM_LIBS && libraries[lib_index].libname[0]) {
		char libbufname[256];
		make_lib_filename(libraries[lib_index].libname,
				libbufname, 256);
		libraries[lib_index].dlhandle = dlopen(libbufname,
				RTLD_LAZY);
		if(!libraries[lib_index].dlhandle)
			fprintf(stderr, "%s\n", dlerror());
		lib_index++;
	}

	buf += 5 + lib_length;
	bufsize -= 5 + lib_length;

	if(setup_functions(buf, bufsize)) {
		ret = 1;
		goto cleanup;
	}
	ret = run_code(buf, bufsize);

cleanup:
	while(lib_index < MAX_NUM_LIBS && libraries[lib_index].libname[0]) {
		if(libraries[lib_index].dlhandle)
			dlclose(libraries[lib_index].dlhandle);
		lib_index++;
	}

	return ret;
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

