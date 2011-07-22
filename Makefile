BINDIR = bin
CFLAGS ?= -O2
CFLAGS += -Wall
CC     = gcc
HAPPY  = happy
HC     = ghc
HFLAGS ?= -Wall -O --make

SRCDIR  = src
STAUDIR = $(SRCDIR)/stau

LDFLAGS += -ldl -lffi

TESTDIR = tests/src
TESTSCORRECTBINDIR = tests/bin/correct
TESTSCORRECTSRCDIR = tests/src/correct
TESTSTYPEERRORBINDIR = tests/bin/type-error
TESTSTYPEERRORSRCDIR = tests/src/type-error
TESTSTYPEINFBINDIR = tests/bin/type-inf
TESTSTYPEINFSRCDIR = tests/src/type-inf

TESTSSTACKSRCDIR = tests/src/stack
TESTSSTACKBINDIR = tests/bin/stack

STACKOBJS = $(addprefix $(SRCDIR)/, stacklib.o stack.o)
STACKGENOBJS = $(addprefix $(SRCDIR)/, stacklib.o stack-gen.o)

all: $(BINDIR)/stack $(BINDIR)/stack-gen $(BINDIR)/stau

$(BINDIR)/stack: $(STACKOBJS)

$(BINDIR)/stack-gen: $(STACKGENOBJS)

$(BINDIR)/stack $(BINDIR)/stack-gen:
	@mkdir -p $(BINDIR)
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $@

$(STACKOBJS) $(STACKGENOBJS): $(SRCDIR)/stack.h $(SRCDIR)/stacklib.h

$(STAUDIR)/ParseStau.hs: $(STAUDIR)/ParseStau.y
	$(HAPPY) -o $@ $<

$(BINDIR)/stau: $(BINDIR) $(STAUDIR)/ParseStau.hs $(STAUDIR)/*.hs
	$(HC) $(HFLAGS) -o $(BINDIR)/stau $(STAUDIR)/*.hs

$(TESTSCORRECTBINDIR):
	@mkdir -p $(TESTSCORRECTBINDIR)

$(TESTSSTACKBINDIR):
	@mkdir -p $(TESTSSTACKBINDIR)

$(TESTSTYPEERRORBINDIR):
	@mkdir -p $(TESTSTYPEERRORBINDIR)

$(TESTSTYPEINFBINDIR):
	@mkdir -p $(TESTSTYPEINFBINDIR)

$(BINDIR):
	@mkdir -p $(BINDIR)

tests: stack-tests correct-tests type-error-tests type-inf-tests ffi-tests

runtests = \
	rm -f $(2)/result-*.txt; \
	for file in $(1)/*.stau; do \
		rm -f $(2)/result-`basename $$file .stau`.txt; \
		$(BINDIR)/stau -o $(2)/`basename $$file .stau` $$file 2>/dev/null && \
		$(BINDIR)/stack-gen < $(2)/`basename $$file .stau` | \
			$(BINDIR)/stack - 2>&1 | sed -e 's/ at 0x[0-9a-f]*/ at 0xdeadbeef/' > \
				$(2)/result-`basename $$file .stau`.txt; \
		echo $$? >> $(2)/result-`basename $$file .stau`.txt; \
		(cmp $(2)/result-`basename $$file .stau`.txt \
			$(1)/correct-`basename $$file .stau`.txt >/dev/null 2>&1 && \
				echo "Test `basename $$file .stau` successful") || \
					echo "Test `basename $$file .stau` failed"; \
	done

runstacktests = \
	rm -f $(2)/result-*.txt; \
	for file in $(1)/*.sta; do \
		rm -f $(2)/result-`basename $$file .sta`.txt; \
		$(BINDIR)/stack-gen < $(1)/`basename $$file` | \
			$(BINDIR)/stack - 2>&1 | sed -e 's/ at 0x[0-9a-f]*/ at 0xdeadbeef/' > \
				$(2)/result-`basename $$file .sta`.txt; \
		echo $$? >> $(2)/result-`basename $$file .sta`.txt; \
		(cmp $(2)/result-`basename $$file .sta`.txt \
			$(1)/correct-`basename $$file .sta`.txt >/dev/null 2>&1 && \
				echo "Test `basename $$file .sta` successful") || \
					echo "Test `basename $$file .sta` failed"; \
	done

stack-tests: $(TESTSSTACKBINDIR) $(BINDIR)/stack $(BINDIR)/stack-gen
	@$(call runstacktests, $(TESTSSTACKSRCDIR), $(TESTSSTACKBINDIR))

correct-tests: $(TESTSCORRECTBINDIR) $(BINDIR)/stau $(BINDIR)/stack $(BINDIR)/stack-gen
	@$(call runtests, $(TESTSCORRECTSRCDIR), $(TESTSCORRECTBINDIR))

type-error-tests: $(TESTSTYPEERRORBINDIR) $(BINDIR)/stau $(BINDIR)/stack $(BINDIR)/stack-gen
	@$(call runtests, $(TESTSTYPEERRORSRCDIR), $(TESTSTYPEERRORBINDIR))

type-inf-tests: $(TESTSTYPEINFBINDIR) $(BINDIR)/stau $(BINDIR)/stack $(BINDIR)/stack-gen
	@$(call runtests, $(TESTSTYPEINFSRCDIR), $(TESTSTYPEINFBINDIR))

ffi-tests: ffi-square

tests/bin/ffi:
	@mkdir -p $@

libsquare.so: tests/bin/ffi tests/src/ffi/square.c
	@$(CC) -shared -Wl,-soname,libsquare.so -o tests/bin/ffi/libsquare.so tests/src/ffi/square.c

ffi-square: $(BINDIR)/stack $(BINDIR)/stack-gen tests/bin/ffi tests/src/ffi/square.sta tests/src/ffi/correct-square.txt libsquare.so
	@$(BINDIR)/stack-gen square < tests/src/ffi/square.sta > tests/bin/ffi/sq.st
	@LD_LIBRARY_PATH=tests/bin/ffi $(BINDIR)/stack tests/bin/ffi/sq.st > tests/bin/ffi/result-square.txt 2>&1
	@(cmp -s tests/src/ffi/correct-square.txt tests/bin/ffi/result-square.txt && echo "ffi-square successful") \
		|| echo "ffi-square failed"

clean:
	rm -rf $(BINDIR) $(STAUDIR)/ParseStau.hs $(STAUDIR)/*.hi $(STAUDIR)/*.o \
		$(TESTSSTACKBINDIR) $(TESTSCORRECTBINDIR) $(TESTSTYPEERRORBINDIR) \
		$(TESTSTYPEINFBINDIR) tests/bin/ffi $(STACKOBJS) $(STACKGENOBJS)

.PHONY: clean tests ffi-tests

