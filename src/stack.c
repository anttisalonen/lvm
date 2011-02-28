#include <stdio.h>

#include "stacklib.h"

void usage(const char *pn)
{
	fprintf(stderr, "Usage: %s <filename>\n",
			pn);
	fprintf(stderr, "\tsimple stack machine\n");
}

int main(int argc, char **argv)
{
	if(argc != 2) {
		usage(argv[0]);
		return 1;
	}
	return parse_file(argv[1]);
}

