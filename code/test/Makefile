# -------------------------------------------------------------------------

PETIT := ../compilo
FLAGS := 
PROGS := $(wildcard *.p)
BASIS := $(patsubst %.p,%,$(PROGS))

.PHONY: test all thorough clean dpp voyage out reftest

.SECONDARY: $(patsubst %.p,%.spi,$(PROGS))

# -------------------------------------------------------------------------

# This entry checks that the PP source program and the MIPS target
# program behave identically. The former is interpreted by the PP
# interpreter; the latter is interpreted by spim.

test: $(patsubst %.p,%.ispim,$(PROGS)) $(patsubst %.p,%.ipp,$(PROGS))
	@for i in $(BASIS) ; do \
	  if diff $$i.ipp $$i.ispim >/dev/null 2>&1 ; then \
	    echo [OK] $$i ; \
	  else \
	    echo [FAILED] $$i ; \
	  fi \
	done

# During a TD, the compiler is typically incomplete and might fail with
# an exception. We hide the error message, so as to be less verbose,
# and create an invalid %.spi file, so as to avoid aborting the entire
# process. This will lead to an invalid %.ispim file and will report
# this particular test as failed.

%.spi: %.p $(PETIT)
	@$(PETIT) $(FLAGS) $< 2>/dev/null || (rm -f $@ && echo "$(PETIT) failed with exit code: $$?" > $@)

%.ispim: %.spi %.in
	spim -file $*.spi < $*.in 2>&1 | (tail -n +6 > $@) 

clean::
	/bin/rm -f *.ispim *.spi

# -------------------------------------------------------------------------

# This entry checks that every intermediate program behaves
# identically as the PP source program. This helps debug the
# interpreters for intermediate languages, and, once these
# interpreters are debugged, helps debug the compiler by
# pinpointing the translation phase that introduces a bug
# into the object program.

all thorough: $(patsubst %.p,%.ipp,$(PROGS))
	@for i in $(BASIS) ; do \
	  echo Now testing: $$i ; \
	  for level in upp rtl cse ertl ltl lin ; do \
	    if $(PETIT) $(FLAGS) -i$$level $$i.p < $$i.in 2>&1 | diff $$i.ipp - ; then \
	      echo [OK] $$level ; \
	    else \
	      echo [FAIL] $$level ; \
	      break ; \
	    fi \
	  done \
	done

# This entry checks that a particular intermediate program behaves
# identically as the PP source program. This helps debug the
# translation to intermediate languages.

upp rtl cse ertl ltl lin: $(patsubst %.p,%.ipp,$(PROGS))
	@for i in $(BASIS) ; do \
	  echo Now testing: $$i ; \
	  if $(PETIT) $(FLAGS) -i$@ $$i.p < $$i.in 2>&1 | diff $$i.ipp - >/dev/null 2>&1 ; then \
	    echo [OK] $@ ; \
	  else \
	    echo [FAIL] $@ ; \
	  fi \
	done

%.ipp: %.p %.in $(PETIT)
	$(PETIT) $(FLAGS) -ipp $*.p < $*.in > $@ 2>&1 || true

clean::
	/bin/rm -f *.ipp

%.iupp: %.p %.in $(PETIT)
	$(PETIT) $(FLAGS) -iupp $*.p < $*.in > $@ 2>&1 || true

clean::
	/bin/rm -f *.iupp

%.irtl: %.p %.in $(PETIT)
	$(PETIT) $(FLAGS) -irtl $*.p < $*.in > $@ 2>&1 || true

clean::
	/bin/rm -f *.irtl

%.icse: %.p %.in $(PETIT)
	$(PETIT) $(FLAGS) -icse $*.p < $*.in > $@ 2>&1 || true

clean::
	/bin/rm -f *.icse

%.iertl: %.p %.in $(PETIT)
	$(PETIT) $(FLAGS) -iertl $*.p < $*.in > $@ 2>&1 || true

clean::
	/bin/rm -f *.iertl

%.iltl: %.p %.in $(PETIT)
	$(PETIT) $(FLAGS) -iltl $*.p < $*.in > $@ 2>&1 || true

clean::
	/bin/rm -f *.iltl

%.ilin: %.p %.in $(PETIT)
	$(PETIT) $(FLAGS) -ilin $*.p < $*.in > $@ 2>&1 || true

clean::
	/bin/rm -f *.ilin

# -------------------------------------------------------------------------

# This entry checks that the PP printer's output is PP parseable
# and is re-printed identically. This is a way of checking that
# that the parser and the pretty-printer are in agreement (their
# composition is the identity). Of course, it is still possible
# for an incorrect parser or pretty-printer to pass this test.

dpp: $(patsubst %.p,%.dpp,$(PROGS))
	@for i in $(BASIS) ; do \
	  if $(PETIT) $(FLAGS) -dpp $$i.dpp | diff $$i.dpp - >/dev/null 2>&1 ; then \
	    echo [OK] $$i ; \
	  else \
	    echo [FAILED] $$i ; \
	  fi \
	done

%.dpp: %.p $(PETIT)
	$(PETIT) $(FLAGS) -dpp $< > $@

clean::
	/bin/rm -f *.dpp

# -------------------------------------------------------------------------

# These entries allow dumping code in intermediate languages.
# The -delimit option is set to make inclusion in the course
# slides easier. The -few option is set to avoid generating
# too much redundant code.

%.pp: %.p $(PETIT)
	$(PETIT) $(FLAGS) -delimit -few -dpp $< > $@

clean::
	/bin/rm -f *.pp

%.upp: %.p $(PETIT)
	$(PETIT) $(FLAGS) -delimit -few -dupp $< > $@

clean::
	/bin/rm -f *.upp

%.rtl: %.p $(PETIT)
	$(PETIT) $(FLAGS) -delimit -few -drtl $< > $@

clean::
	/bin/rm -f *.rtl

%.cse: %.p $(PETIT)
	$(PETIT) $(FLAGS) -delimit -few -dcse $< > $@

clean::
	/bin/rm -f *.cse

%.ertl: %.p $(PETIT)
	$(PETIT) $(FLAGS) -delimit -few -dertl $< > $@

clean::
	/bin/rm -f *.ertl

%.ltl: %.p $(PETIT)
	$(PETIT) $(FLAGS) -delimit -few -dltl $< > $@

clean::
	/bin/rm -f *.ltl

%.lin: %.p $(PETIT)
	$(PETIT) $(FLAGS) -delimit -few -dlin $< > $@

clean::
	/bin/rm -f *.lin

%.dot: %.p $(PETIT)
	$(PETIT) $(FLAGS) -few -dgraph f $< > $@

%.eps: %.dot
	circo -Tps $< > $@

%.pdf: %.eps
	epstopdf -o=$@ $<

%.live: %.p $(PETIT)
	$(PETIT) $(FLAGS) -delimit -few -dertl -dlive $< > $@

voyage: fact.pp fact.upp fact.rtl fact.cse fact.ertl fact.ltl fact.lin fact.spi fact.live fact.pdf

# -------------------------------------------------------------------------

# This entry generates the .out files, which contain the expected
# output of each test program. Of course, this makes sense only if
# the compiler is known to be correct. The .out file is just a renamed
# version of the .ipp file.

out: $(patsubst %.p,%.out,$(PROGS))

%.out: %.ipp
	cp -f $< $@

# This entry compares the .ipp files (generated by the student's
# compiler) with the .out files (generated offline by the reference
# compiler). This ensures that the programs produced by the student's
# compiler have the expected behavior.

reftest: $(patsubst %.p,%.ipp,$(PROGS)) # intentionally do not re-generate the .out files!
	@for i in $(BASIS) ; do \
	  if diff $$i.ipp $$i.out >/dev/null 2>&1 ; then \
	    echo [OK] $$i ; \
	  else \
	    echo [FAILED] $$i ; \
	  fi \
	done

# Intentionally do not remove the .out files upon make clean.

