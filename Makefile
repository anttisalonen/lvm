BINDIR = bin
CFLAGS ?= -Wall -O2
CC     = gcc
HAPPY  = happy
HC     = ghc
HFLAGS ?= -Wall -O --make

SRCDIR  = src
STAUDIR = $(SRCDIR)/stau

all: stack stack-gen stau

$(BINDIR):
	mkdir -p $(BINDIR)

stack: $(BINDIR)
	$(CC) $(CFLAGS) -o $(BINDIR)/$@ $(SRCDIR)/stacklib.c $(SRCDIR)/stack.c

stack-gen: $(BINDIR)
	$(CC) $(CFLAGS) -o $(BINDIR)/$@ $(SRCDIR)/stacklib.c $(SRCDIR)/stack-gen.c


$(STAUDIR)/ParseStau.hs: $(STAUDIR)/ParseStau.y
	$(HAPPY) -o $@ $<

stau: $(BINDIR) $(STAUDIR)/ParseStau.hs
	$(HC) $(HFLAGS) -o $(BINDIR)/stau $(STAUDIR)/ParseStau.hs $(STAUDIR)/Stau.hs $(STAUDIR)/Main.hs

clean:
	rm -rf $(BINDIR) $(STAUDIR)/ParseStau.hs $(STAUDIR)/*.hi $(STAUDIR)/*.o

.PHONY: clean all

