#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "stack.h"
#include "stacklib.h"

long int get_two_nums(const char *buf, int *a, int *b)
{
	char *endptr;
	*a = strtol(buf, &endptr, 0);
	if(*endptr && *endptr == ' ') {
		*b = strtol(endptr, &endptr, 0);
		return !(*endptr && *endptr != '\n');
	}
	else {
		return 0;
	}
}

long int getnum(const char *buf, int *succ)
{
	char *endptr;
	long int val = strtol(buf, &endptr, 0);
	if(*endptr && *endptr != '\n') {
		*succ = 0;
		return 0;
	}
	else {
		*succ = 1;
		return val;
	}
}

void num_to_be(long int val, int bytes, char *buf)
{
	char v = 0;
	if(val < 0) {
		v = 0x80;
		val = -val;
	}
	v |= (val & 0x7f);
	sprintf(buf, "%c", v);
	int i;
	val >>= 7;
	for(i = 1; i < bytes; i++) {
		v = val & 0xff;
		buf[i] = v;
		val >>= 8;
	}
}

void int_to_be(long int val, char *buf)
{
	num_to_be(val, 4, buf);
}

static int interactive;
static char interactive_buffer[1024];
static int interactive_bufferpos = 0;

#define OUTPUT_BUFFER_SIZE 1024
#define MAX_NUM_LABELS 26

static int current_addr = 0;
static int output_buffer_offset = 0;
static char output_buffer[OUTPUT_BUFFER_SIZE];

static int labels[MAX_NUM_LABELS];
static int label_offsets[MAX_NUM_LABELS];

void output_char(char ch)
{
	if(interactive) {
		if(interactive_bufferpos > 1000) {
			fprintf(stderr, "Buffer full\n");
			return;
		}
		interactive_bufferpos += sprintf(interactive_buffer + interactive_bufferpos,
				"%c", ch);
	}
	else {
		output_buffer[output_buffer_offset++] = ch;
		current_addr++;
		if(output_buffer_offset >= OUTPUT_BUFFER_SIZE) {
			fprintf(stderr, "Output buffer full\n");
			exit(1);
		}
	}
}

void output_ffiname(const char *str)
{
	while(!isspace(*str)) {
		output_char(*str);
		str++;
	}
}

void output_nums(char opcode, int a, int b)
{
	char buf[4];
	int i;
	output_char(opcode);
	num_to_be(a, 2, buf);
	for(i = 0; i < 2; i++)
		output_char(buf[i]);
	num_to_be(b, 2, buf);
	for(i = 0; i < 2; i++)
		output_char(buf[i]);
}

void output_be(long int num)
{
	char buf[4];
	int i;
	int_to_be(num, buf);
	for(i = 0; i < 4; i++) {
		output_char(buf[i]);
	}
}

void output_num(char opcode, long int num)
{
	output_char(opcode);
	output_be(num);
}

void output_to_label(char opcode, int label_index)
{
	label_offsets[label_index] = output_buffer_offset;
	output_num(opcode, 0);
}

void output(char opcode)
{
	output_char(opcode);
}

void sync_output(void)
{
	int i;
	int bufsize = output_buffer_offset;
	for(i = 0; i < MAX_NUM_LABELS; i++) {
		if(labels[i] != 0) {
			output_buffer_offset = label_offsets[i] + 1;
			output_be(labels[i]);
			current_addr -= 4;
		}
	}
	for(i = 0; i < bufsize; i++) {
		fprintf(stdout, "%c", output_buffer[i]);
	}
	output_buffer_offset = 0;
}

int funend(char opcode, int fundef)
{
	if(fundef) {
		fundef = 0;
		output(opcode);
		sync_output();
	}
	else {
		fprintf(stderr, "?\n");
	}
	return fundef;
}

void reset_labels(void)
{
	memset(labels, 0x00, sizeof(labels));
	memset(label_offsets, 0x00, sizeof(label_offsets));
}

int output_offset(const char *buf, const char *mnemonic,
		int opcode)
{
	char mn_buf[20];
	int mlen = strlen(mnemonic);
	snprintf(mn_buf, 19, "%s ", mnemonic);
	mn_buf[19] = '\0';
	if(!strncmp(buf, mn_buf, mlen + 1)) {
		int succ;
		long int parsed_num = getnum(buf + mlen + 1, &succ);
		if(!succ) {
			if(buf[mlen + 1] >= 'a' && buf[mlen + 1] <= 'z') {
				output_to_label(opcode, buf[mlen + 1] - 'a');
			}
			else {
				fprintf(stderr, "?\n");
			}
		}
		else {
			output_num(opcode, parsed_num);
		}
		return 1;
	}
	else
		return 0;
}

int handle_ffidef(const char *buf)
{
	int parsed_funnum;
	unsigned char types[MAX_NUM_FFI_PARAMETERS];
	int i = 0, j = 0;
	long int val;
	const char *numptr = buf;
	memset(types, 0x00, sizeof(types));
	char *endptr;
	parsed_funnum = strtol(numptr, &endptr, 0);
	if(endptr == numptr || !parsed_funnum)
		return 0;
	numptr++;
	do {
		val = strtol(numptr, &endptr, 0);
		if(endptr == numptr)
			return 0;
		types[i] = val;
		numptr = endptr + 1;
		i++;
	} while(i < sizeof(types) - 1 && (!i || val != 0));
	output_nums(OPCODE_FFIDEF, parsed_funnum, types[0]);
	for(j = 1; j < i; j += 4) {
		output_char(types[j]);
		if(j + 1 < i)
			output_char(types[j + 1]);
		else
			output_char(0);
		if(j + 2 < i)
			output_char(types[j + 2]);
		else
			output_char(0);
		if(j + 3 < i)
			output_char(types[j + 3]);
		else
			output_char(0);
	}
	output_ffiname(numptr);
	sync_output();
	reset_labels();
	return 1;
}

int main(int argc, char **argv)
{
	char buf[1024];
	int fundef = 0;
	interactive = argc > 1 && !strncmp(argv[1], "-i", 2);
	reset_labels();
	while(1) {
		int emptyline;
		if(interactive) {
			printf(">>> ");
			fflush(stdout);
		}
		emptyline = (fgets(buf, 1024, stdin) == NULL);
		if(!interactive && emptyline)
			break;
		if(buf[0] == ':') {
			if(buf[1] >= 'a' && buf[1] <= 'z') {
				labels[buf[1] - 'a'] = current_addr;
			}
		}
		else if(!strncmp(buf, "ADD", 3)) {
			output(OPCODE_ADD);
		}
		else if(!strncmp(buf, "SUB", 3)) {
			output(OPCODE_SUB);
		}
		else if(!strncmp(buf, "MUL", 3)) {
			output(OPCODE_MUL);
		}
		else if(!strncmp(buf, "DIV", 3)) {
			output(OPCODE_DIV);
		}
		else if(!strncmp(buf, "LT", 2)) {
			output(OPCODE_LT);
		}
		else if(!strncmp(buf, "LE", 2)) {
			output(OPCODE_LE);
		}
		else if(!strncmp(buf, "EQ", 2)) {
			output(OPCODE_EQ);
		}
		else if(!strncmp(buf, "DUP", 3)) {
			output(OPCODE_DUP);
		}
		else if(!strncmp(buf, "DROP", 4)) {
			output(OPCODE_DROP);
		}
		else if(!strncmp(buf, "NOP", 3)) {
			output(OPCODE_NOP);
		}
		else if(!strncmp(buf, "SWAP", 4)) {
			output(OPCODE_SWAP);
		}
		else if(!strncmp(buf, "NEW", 3)) {
			output(OPCODE_NEW);
		}
		else if(!strncmp(buf, "RSTORE", 6)) {
			output(OPCODE_RSTORE);
		}
		else if(!strncmp(buf, "RLOAD", 4)) {
			output(OPCODE_RLOAD);
		}
		else if(!strncmp(buf, "END_THUNK", 9)) {
			output(OPCODE_END_THUNK);
		}
		else if(output_offset(buf, "BR", OPCODE_BRANCH));
		else if(output_offset(buf, "BRNZ", OPCODE_BRANCHNZ));
		else if(output_offset(buf, "THUNK", OPCODE_START_THUNK));
		else if(!strncmp(buf, "FUNCALL ", 8)) {
			int succ;
			long int parsed_num = getnum(buf + 8, &succ);
			if(!succ)
				fprintf(stderr, "?\n");
			else {
				output_num(OPCODE_CALLFUN, parsed_num);
			}
		}
		else if(!strncmp(buf, "FUNPCALL", 8)) {
			output(OPCODE_CALLPFUN);
		}
		else if(!strncmp(buf, "FUNDEF ", 7)) {
			if(fundef) {
				fprintf(stderr, "?\n");
			}
			else {
				int parsed_funnum, parsed_params;
				int succ = get_two_nums(buf + 7, &parsed_funnum, &parsed_params);
				if(!succ)
					fprintf(stderr, "?\n");
				else {
					output_nums(OPCODE_DEFUN_START, parsed_params, parsed_funnum);
					fundef = 1;
					reset_labels();
				}
			}
		}
		else if(!strncmp(buf, "FUNEND", 6)) {
			fundef = funend(OPCODE_DEFUN_END, fundef);
		}
		else if(!strncmp(buf, "RET0", 4)) {
			fundef = funend(OPCODE_RET0, fundef);
		}
		else if(!strncmp(buf, "RET1", 4)) {
			fundef = funend(OPCODE_RET1, fundef);
		}
		else if(!strncmp(buf, "LOAD ", 5)) {
			int succ;
			long int parsed_num = getnum(buf + 5, &succ);
			if(!succ)
				fprintf(stderr, "?\n");
			else
				output_num(OPCODE_LOAD, parsed_num);
		}
		else if(!strncmp(buf, "FFIDEF ", 7)) {
			if(fundef) {
				fprintf(stderr, "?\n");
			}
			else {
				int succ = handle_ffidef(buf + 7);
				if(!succ) {
					fprintf(stderr, "FFIDEF?\n");
				}
			}
		}
		else if(buf[0] == 'f') {
			int succ;
			long int parsed_num = getnum(buf + 1, &succ);
			if(!succ)
				fprintf(stderr, "?\n");
			else
				output_num(OPCODE_PFUN_ID, parsed_num);
		}
		else if(buf[0] == '-' || isdigit(buf[0])) {
			int succ;
			long int parsed_num = getnum(buf, &succ);
			if(!succ)
				fprintf(stderr, "?\n");
			else
				output_num(OPCODE_INT, parsed_num);
		}
		else if(interactive && !strncmp(buf, "run", 3)) {
			parse_buffer(interactive_buffer,
					interactive_bufferpos);
		}
		else if(buf[0]) {
			fprintf(stderr, "?\n");
		}
	}
	return 0;
}

