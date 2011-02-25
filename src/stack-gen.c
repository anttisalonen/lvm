#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "stack.h"

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

void int_to_be(long int val, char *buf)
{
	char v = 0;
	if(val < 0) {
		v = 0x80;
		val = -val;
	}
	v |= (val & 0x7f);
	sprintf(buf, "%c", v);
	int i;
	val >>= 8;
	for(i = 1; i < 4; i++) {
		v = val & 0xff;
		buf[i] = v;
		val >>= 8;
	}
}

void output_num(char opcode, long int num)
{
	char buf[4];
	fprintf(stdout, "%c", opcode);
	int_to_be(num, buf);
	int i;
	for(i = 0; i < 4; i++)
		fprintf(stdout, "%c", buf[i]);
}

int main(int argc, char **argv)
{
	char buf[1024];
	int fundef = 0;
	while(fgets(buf, 1024, stdin)) {
		if(!strncmp(buf, "ADD", 3)) {
			fprintf(stdout, "%c", OPCODE_ADD);
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
				int succ;
				long int parsed_num = getnum(buf + 7, &succ);
				if(!succ)
					fprintf(stderr, "?\n");
				else {
					output_num(OPCODE_DEFUN_START, parsed_num);
					fundef = 1;
				}
			}
		}
		else if(!strncmp(buf, "FUNEND", 6)) {
			if(fundef) {
				fundef = 0;
				fprintf(stdout, "%c", OPCODE_DEFUN_END);
			}
			else
				fprintf(stderr, "?\n");
		}
		else {
			int succ;
			long int parsed_num = getnum(buf, &succ);
			if(!succ)
				fprintf(stderr, "?\n");
			else
				output_num(OPCODE_INT, parsed_num);
		}
	}
	return 0;
}
