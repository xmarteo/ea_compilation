# -------------------------------------------------------------------------

# Define which source files are generated, which modules are linked into
# the executable. Define the executable's name.

GENERATED      := parser.ml parser.mli lexer.ml

MODULES        := myMap integer misc 			\
		  stringSet stringMap setMap prioritySet\
		  unionFind print atom			\
	          option settings location error	\
		  MIPS printOps interpretMIPS		\
                  printPrimitive 			\
		  parser lexer typechecking lpp2pp	\
		  printPP interpretPP			\
		  upp2upp pp2upp printUPP interpretUPP  \
	          register label upp2rtlI upp2rtl cse	\
		  printCFG printRTL interpretRTL	\
	          rtl2ertlI rtl2ertl Fix liveness	\
		  printERTL interpretERTL	 	\
	          interference zero build uses		\
		  coloring spill ertl2ltlI ertl2ltl	\
		  printLTL interpretLTL			\
		  branch ltl2linI ltl2lin		\
		  printLIN interpretLIN			\
		  lin2asm printASM main

EXECUTABLE     := compilo

# -------------------------------------------------------------------------

# Override some settings (compilation flags).

BFLAGS         := -dtypes -g
OFLAGS         := -dtypes
BLNKFLAGS      := -g
PGFLAGS        := --infer -v

# -------------------------------------------------------------------------

# Include our standard set of rules.

MENHIR := menhir 
include Makefile.shared

# -------------------------------------------------------------------------

# Indicate how to build the parser.

parser.ml parser.mli: parser.mly parser.d
	@if [ -f parser.ml ] ; then /bin/chmod +w parser.ml ; fi
	@if [ -f parser.mli ] ; then /bin/chmod +w parser.mli ; fi
	@rm -f parser.conflicts parser.automaton
	$(MENHIR) --ocamlc "$(OCAMLC)" $(PGFLAGS) parser.mly
	@/bin/chmod -w parser.ml parser.mli

parser.d: parser.mly
	@if [ -f parser.ml ] ; then /bin/chmod +w parser.ml ; fi
	@if [ -f parser.mli ] ; then /bin/chmod +w parser.mli ; fi
	$(OCAML) $(OCAMLDEPWRAPPER) $(GENERATED) - \
		 $(MENHIR) --ocamldep "$(OCAMLDEP)" --depend parser.mly > $@

ifeq ($(findstring $(MAKECMDGOALS),$(COLD)),)
-include parser.d
endif

# -------------------------------------------------------------------------
# Testing.

.PHONY: test debug dpp thorough upp rtl cse ertl ltl lin reftest

test dpp thorough upp rtl cse ertl ltl lin reftest: $(EXECUTABLE)
	$(MAKE) -C test $(MFLAGS) $@

debug: $(EXECUTABLE)$(BSUFFIX)
	$(MAKE) OCAMLRUNPARAM="b=1" PETIT="../$(EXECUTABLE)$(BSUFFIX)" -C test $(MFLAGS)

clean::
	$(MAKE) -C test $(MFLAGS) $@

# -------------------------------------------------------------------------

# Define how to count lines.

.PHONY: wc

wc:
	/bin/rm -f $(GENERATED)
	ocamlwc -e -p *.ml *.mli

