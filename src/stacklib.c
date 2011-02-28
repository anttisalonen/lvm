#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "stack.h"

enum opcode {
	opcode_add,
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
static int return_points[MAX_CALL_DEPTH];

static int get_int(const char *buf, int *pc, stackvalue *sv)
{
	sv->vt = valuetype_int;
	sv->value.intvalue = 0;
	sv->value.intvalue = (buf[*pc]) & 0x7f;
	int i;
	for(i = 1; i < 4; i++) {
		sv->value.intvalue += (buf[*pc + i]) << (i * 8);
	}
	if(buf[*pc] & 0x80)
		sv->value.intvalue = -sv->value.intvalue;
	(*pc) += 4;
	return 0;
}

static int get_fun_value(const char *buf, int *pc, unsigned int bufsize)
{
	switch(buf[*pc]) {
		case OPCODE_ADD:
			(*pc)++;
			return 0;
		case OPCODE_INT:
		case OPCODE_CALLFUN:
		case OPCODE_BRANCH:
		case OPCODE_BRANCHNZ:
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

static int get_value(const char *buf, stackvalue *sv, int *pc, unsigned int bufsize,
		int *interp, stackvalue *stack, int *sp)
{
	*interp = 1;
	switch(buf[*pc]) {
		case OPCODE_ADD:
			sv->vt = valuetype_opcode;
			sv->value.op = opcode_add;
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
				(*pc) = return_points[--return_depth];
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
			return_points[return_depth++] = *pc;
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
			if(i1) {
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
		default:
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
					if(*sp < 2)
						return 1;
					if(!stackvalue_is_int(&stack[*sp - 1]))
						return 1;
					if(!stackvalue_is_int(&stack[*sp - 2]))
						return 1;
					int32_t i1 = stack[*sp - 1].value.intvalue;
					int32_t i2 = stack[*sp - 2].value.intvalue;
					(*sp)--;
					stack[*sp - 1].vt = valuetype_int;
					stack[*sp - 1].value.intvalue = i1 + i2;
					// printf("[%d] %d + %d = %d\n", *sp, i1, i2, stack[*sp - 1].value.intvalue);
					return 0;
			}
		default:
			return 1;
	}
}

#define STACK_SIZE 1024

static int run_code(const char *buf, unsigned int bufsize)
{
	int pc = functions[0];
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
			fprintf(stderr, "Invalid instruction\n");
			return 1;
		}
		if(interp) {
			if(interpret(&sv, stack, &sp)) {
				fprintf(stderr, "Invalid instruction\n");
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
			if(*current_fundef)
				return 1;
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


