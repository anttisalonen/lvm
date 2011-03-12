BINDIR = bin
CFLAGS ?= -Wall -O2
CC     = gcc
HAPPY  = happy
HC     = ghc
HFLAGS ?= -Wall -O --make

SRCDIR  = src
STAUDIR = $(SRCDIR)/stau

TESTDIR = tests/src
TESTSBINDIR = tests/bin

all: stack stack-gen stau

$(BINDIR):
	mkdir -p $(BINDIR)

stack: $(BINDIR)/stack

$(BINDIR)/stack: $(BINDIR)
	$(CC) $(CFLAGS) -o $@ $(SRCDIR)/stacklib.c $(SRCDIR)/stack.c

stack-gen: $(BINDIR)/stack-gen

$(BINDIR)/stack-gen: $(BINDIR)
	$(CC) $(CFLAGS) -o $@ $(SRCDIR)/stacklib.c $(SRCDIR)/stack-gen.c

$(STAUDIR)/ParseStau.hs: $(STAUDIR)/ParseStau.y
	$(HAPPY) -o $@ $<

stau: $(BINDIR)/stau

$(BINDIR)/stau: $(BINDIR) $(STAUDIR)/ParseStau.hs $(STAUDIR)/*.hs
	$(HC) $(HFLAGS) -o $(BINDIR)/stau $(STAUDIR)/ParseStau.hs $(STAUDIR)/Stau.hs $(STAUDIR)/Main.hs

$(TESTSBINDIR):
	mkdir -p $(TESTSBINDIR)

tests: $(TESTSBINDIR) stau stack stack-gen
	rm -f $(TESTSBINDIR)/results.txt
	for file in $(TESTDIR)/*.stau; do \
		$(BINDIR)/stau -o $(TESTSBINDIR)/`basename $$file .stau` $$file; \
		$(BINDIR)/stack-gen < $(TESTSBINDIR)/`basename $$file .stau` > $(TESTSBINDIR)/a.out; \
		$(BINDIR)/stack $(TESTSBINDIR)/a.out | tee -a $(TESTSBINDIR)/results.txt; \
	done
	cmp $(TESTDIR)/results.txt $(TESTSBINDIR)/results.txt

clean:
	rm -rf $(BINDIR) $(STAUDIR)/ParseStau.hs $(STAUDIR)/*.hi $(STAUDIR)/*.o $(TESTSBINDIR)

.PHONY: clean all

