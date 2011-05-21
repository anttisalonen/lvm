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

void output_nums(char opcode, int a, int b)
{
	char buf[4];
	int i;
	output_char(opcode);
	num_to_be(a, 2, buf);
	for(i = 0; i < 2; i++) {
		output_char(buf[i]);
	}
	num_to_be(b, 2, buf);
	for(i = 0; i < 2; i++) {
		output_char(buf[i]);
	}
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
	label_offsets[label_index] = current_addr;
	output_num(opcode, 0);
}

void output(char opcode)
{
	output_char(opcode);
}

void flush_output(void)
{
	int i;
	int bufsize = output_buffer_offset;
	for(i = 0; i < MAX_NUM_LABELS; i++) {
		if(labels[i] != 0) {
			output_buffer_offset = label_offsets[i] + 1;
			output_be(labels[i]);
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
		flush_output();
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
		else if(!strncmp(buf, "BR ", 3)) {
			int succ;
			long int parsed_num = getnum(buf + 3, &succ);
			if(!succ) {
				if(buf[3] >= 'a' && buf[3] <= 'z') {
					output_to_label(OPCODE_BRANCH, buf[3] - 'a');
				}
				else {
					fprintf(stderr, "?\n");
				}
			}
			else {
				output_num(OPCODE_BRANCH, parsed_num);
			}
		}
		else if(!strncmp(buf, "BRNZ ", 5)) {
			int succ;
			long int parsed_num = getnum(buf + 5, &succ);
			if(!succ) {
				if(buf[5] >= 'a' && buf[5] <= 'z') {
					output_to_label(OPCODE_BRANCHNZ, buf[5] - 'a');
				}
				else {
					fprintf(stderr, "?\n");
				}
			}
			else {
				output_num(OPCODE_BRANCHNZ, parsed_num);
			}
		}
		else if(!strncmp(buf, "FUNCALL ", 8)) {
			int succ;
			long int parsed_num = getnum(buf + 8, &succ);
			if(!succ)
				fprintf(stderr, "?\n");
			else {
				output_num(OPCODE_CALLFUN, parsed_num);
			}
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

