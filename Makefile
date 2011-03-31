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

STACKOBJS = $(addprefix $(SRCDIR)/, stacklib.o stack.o)
STACKGENOBJS = $(addprefix $(SRCDIR)/, stacklib.o stack-gen.o)

all: $(BINDIR)/stack $(BINDIR)/stack-gen $(BINDIR)/stau

$(BINDIR)/stack: $(STACKOBJS)

$(BINDIR)/stack-gen: $(STACKGENOBJS)

$(BINDIR)/stack $(BINDIR)/stack-gen:
	@mkdir -p $(BINDIR)
	$(CC) $(CFLAGS) $^ -o $@

$(STACKOBJS) $(STACKGENOBJS): $(SRCDIR)/stack.h $(SRCDIR)/stacklib.h

$(STAUDIR)/ParseStau.hs: $(STAUDIR)/ParseStau.y
	$(HAPPY) -o $@ $<

$(BINDIR)/stau: $(BINDIR) $(STAUDIR)/ParseStau.hs $(STAUDIR)/*.hs
	$(HC) $(HFLAGS) -o $(BINDIR)/stau $(STAUDIR)/ParseStau.hs $(STAUDIR)/Stau.hs $(STAUDIR)/Main.hs

$(TESTSBINDIR):
	mkdir -p $(TESTSBINDIR)

tests: $(TESTSBINDIR) $(BINDIR)/stau $(BINDIR)/stack $(BINDIR)/stack-gen
	rm -f $(TESTSBINDIR)/results.txt
	for file in $(TESTDIR)/*.stau; do \
		$(BINDIR)/stau -o $(TESTSBINDIR)/`basename $$file .stau` $$file; \
		$(BINDIR)/stack-gen < $(TESTSBINDIR)/`basename $$file .stau` > $(TESTSBINDIR)/a.out; \
		$(BINDIR)/stack $(TESTSBINDIR)/a.out | tee -a $(TESTSBINDIR)/results.txt; \
	done
	@cmp $(TESTDIR)/results.txt $(TESTSBINDIR)/results.txt

clean:
	rm -rf $(BINDIR) $(STAUDIR)/ParseStau.hs $(STAUDIR)/*.hi $(STAUDIR)/*.o $(TESTSBINDIR)

.PHONY: clean tests

