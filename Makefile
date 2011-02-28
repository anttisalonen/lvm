BINDIR = bin
CFLAGS ?= -Wall -O2
SRCDIR = src
CC     = gcc

all: stack stack-gen

$(BINDIR):
	mkdir -p $(BINDIR)

stack: $(BINDIR)
	$(CC) $(CFLAGS) -o $(BINDIR)/$@ $(SRCDIR)/stacklib.c $(SRCDIR)/stack.c

stack-gen: $(BINDIR)
	$(CC) $(CFLAGS) -o $(BINDIR)/$@ $(SRCDIR)/stacklib.c $(SRCDIR)/stack-gen.c

clean:
	rm -rf $(BINDIR)

