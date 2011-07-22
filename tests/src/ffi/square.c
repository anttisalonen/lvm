#include <stdio.h>

int square(int x)
{
	fprintf(stderr, "I, square, was called!\n");
	return x * x;
}

