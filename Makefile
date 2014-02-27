# 
# Rules for compiling and linking the Sage typechecker/evaluator
#
# Type
#   make         to rebuild the executable file sage
#   make windows to rebuild the executable file sage.exe
#   make test    to rebuild the executable and run it on tests
#   make clean   to remove all intermediate and temporary files
#   make depend  to rebuild the intermodule dependency graph that is used
#                  by make to determine which order to schedule 
#	           compilations.  You should not need to do this unless
#                  you add new modules or new dependencies between 
#                  existing modules.  (The graph is stored in the file
#                  .depend)

# These are the object files needed to rebuild the main executable file
#
OCAMLC=$(if $(shell which ocamlc.opt),ocamlc.opt,ocamlc)
NOCAMLC=$(if $(shell which ocamlopt.opt),ocamlopt.opt,ocamlopt)

MLFILES = \
	src/support.ml \
	src/options.ml \
	src/syntax.ml \
	src/print.ml \
	src/primitives.ml \
	src/parser.ml \
	src/lexer.ml \
	src/eval.ml \
	src/gcenv.ml \
	src/db.ml \
	src/subtype.ml \
	src/prim_defs.ml\
	src/compile.ml \
	src/formulaparse.ml \
	src/simplify.ml \
	src/sformula.ml \
	src/basicalg.ml 

MLIFILES = $(filter-out src/lexer.mli, $(patsubst %.ml,%.mli, $(MLFILES)))
CMIFILES = $(patsubst %.ml,%.cmi, $(MLFILES))
OBJS = $(patsubst %.ml,%.cmo, $(MLFILES)) 
NOBJS = $(patsubst %.ml,%.cmx, $(MLFILES))

DEPEND += src/lexer.ml src/parser.ml 

# Files that need to be generated from other files

OCAMLOPTS = unix.cma str.cma -g -I src
NOCAMLOPTS = unix.cmxa str.cmxa -p -I src

DB = cow.db

# When "make" is invoked with no arguments, we build an executable 
# typechecker, after building everything that it depends on
all: sage unittest pdb platex

# On a Windows machine, we do exactly the same except that the executable
# file that gets built needs to have the extension ".exe"
windows: f.exe

# Build an executable typechecker
sage: $(OBJS) src/main.cmo 
	@echo Linking $@
	$(OCAMLC) $(OCAMLOPTS) -o $@ $^

sage.opt: $(NOBJS) src/main.cmx
	@echo Linking $@
	$(NOCAMLC) $(NOCAMLOPTS) -o $@ $^

unittest: $(OBJS) src/unittest.cmo
	@echo Linking $@
	$(OCAMLC) $(OCAMLOPTS) -o $@ $^

pdb: $(OBJS) src/pdb.cmo
	@echo Linking $@
	$(OCAMLC) $(OCAMLOPTS) -o $@ $^

platex: $(OBJS) src/platex.cmo
	$(OCAMLC) $(OCAMLOPTS) -o $@ $^

# Build an executable typechecker for Windows
sage.exe: $(OBJS) src/main.cmo 
	@echo Linking $@
	$(OCAMLC) $(OCAMLOPTS) -o $@ $^

# Create a dependency graph, in postscript format, for the modules
depgraph:
	ocamldoc -dot *.mli *.ml -o depgraph.dot
	dot -Tps depgraph.dot > depgraph.ps

tydepgraph:
	ocamldoc -dot -dot-types *.mli *.ml -o tydepgraph.dot
	dot -Tps tydepgraph.dot > tydepgraph.ps

# Build and test
test: all
	@ ./unittest
	@ rm -f $(DB)
	@ cd refute && $(MAKE)
	@ cd tests && $(MAKE)

docs: all
	ocamldoc -v -html -d doc/ocamldoc -keep-code -sort \
	-all-params -colorize-code -I src $(MLIFILES) $(MLFILES)

# Compile an ML module interface
%.cmi : %.mli
	$(OCAMLC) -c $(OCAMLOPTS) $<

# Compile an ML module implementation to bytecode
%.cmo : %.ml
	$(OCAMLC) -c $(OCAMLOPTS) $<

# Compile an ML module implementation to native code
%.cmx : %.ml
	$(NOCAMLC) -c $(NOCAMLOPTS) $<

# Generate ML files from a parser definition file
%.ml %.mli: %.mly
#	@rm -f $^
	ocamlyacc -v $< 
#	@chmod -w $^

# Generate ML files from a lexer definition file
%.ml %.mli: %.mll
	@rm -f $@
	ocamllex $<
	@chmod -w $@

# Clean up the directory
clean::
	rm -rf $(OBJS) $(NOBJS) $(CMIFILES) \
		src/lexer.ml src/parser.ml src/parser.mli src/parser.output \
		sage unittest pdb TAGS src/*~ *.bak depgraph.dot depgraph.ps \
		sage.opt sage.exe pdb.exe unittest.exe platex.exe \
		$(DB) \
		src/*.cmo src/*.cmx src/*.cmi \
		platex \
		doc/ocamldoc/*
	cd tests && make clean
	cd refute && make clean

# Rebuild intermodule dependencies
depend:: $(DEPEND) 
	ocamldep $(INCLUDE) $(MLIFILES) $(MLFILES) > .depend

# Word count
wc:: clean
	wc $(MLFILES) $(MLIFILES) src/parser.mly src/lexer.mll 

# Include an automatically generated list of dependencies between source files
include .depend
