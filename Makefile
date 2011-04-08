BINDIR = bin
CFLAGS ?= -Wall -O2
CC     = gcc
HAPPY  = happy
HC     = ghc
HFLAGS ?= -Wall -O --make

SRCDIR  = src
STAUDIR = $(SRCDIR)/stau

TESTDIR = tests/src
TESTSCORRECTBINDIR = tests/bin/correct
TESTSCORRECTSRCDIR = tests/src/correct
TESTSTYPEERRORBINDIR = tests/bin/type-error
TESTSTYPEERRORSRCDIR = tests/src/type-error

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

$(TESTSCORRECTBINDIR):
	mkdir -p $(TESTSCORRECTBINDIR)

$(TESTSTYPEERRORBINDIR):
	mkdir -p $(TESTSTYPEERRORBINDIR)

$(BINDIR):
	mkdir -p $(BINDIR)

tests: correct-tests type-error-tests

runtests = \
	rm -f $(2)/result-*.txt; \
	for file in $(1)/*.stau; do \
		rm -f $(2)/result-`basename $$file .stau`.txt; \
		$(BINDIR)/stau -o $(2)/`basename $$file .stau` $$file 2>/dev/null && \
		$(BINDIR)/stack-gen < $(2)/`basename $$file .stau` | \
			$(BINDIR)/stack - > $(2)/result-`basename $$file .stau`.txt; \
		echo $$? >> $(2)/result-`basename $$file .stau`.txt; \
		(cmp $(2)/result-`basename $$file .stau`.txt \
			$(1)/correct-`basename $$file .stau`.txt >/dev/null 2>&1 && \
				echo "Test `basename $$file .stau` successful") || \
					echo "Test `basename $$file .stau` failed"; \
	done

correct-tests: $(TESTSCORRECTBINDIR) $(BINDIR)/stau $(BINDIR)/stack $(BINDIR)/stack-gen
	@$(call runtests, $(TESTSCORRECTSRCDIR), $(TESTSCORRECTBINDIR))

type-error-tests: $(TESTSTYPEERRORBINDIR) $(BINDIR)/stau $(BINDIR)/stack $(BINDIR)/stack-gen
	@$(call runtests, $(TESTSTYPEERRORSRCDIR), $(TESTSTYPEERRORBINDIR))

clean:
	rm -rf $(BINDIR) $(STAUDIR)/ParseStau.hs $(STAUDIR)/*.hi $(STAUDIR)/*.o \
		$(TESTSCORRECTBINDIR) $(TESTSTYPEERRORBINDIR)

.PHONY: clean tests

