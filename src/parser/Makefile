TOP=../../..

SRC = spider_monkey_ast.ml parse_error.ml lexer_flow.ml parser_flow.ml

INCLUDEDIRS=

LIBS= 

OCAMLCFLAGS+=-w +a-4-6-29-35-44-48-27 -warn-error +a

-include $(TOP)/Makefile.common

.PHONY:: tools test

all:: all.opt
all.opt: parser_flow.cmxa flow.js
all.bc: parser_flow.cma flow.js

test: all
	./test/run_esprima_tests.js
	./test/run_hardcoded_tests.js

tools: parser_flow.cmxa
	$(MAKE) -C tools all

lexer_flow.ml: lexer_flow.mll
	ocamllex $<

parser_flow.cma: $(OBJS) parser_flow.cmo
	$(OCAMLC) -a -o $@ $^

parser_flow.cmxa: $(OPTOBJS) parser_flow.cmx
	$(OCAMLOPT) -a -o $@ $^

# This lives in a different directory so that make depend works
flow_js.cmo: $(OBJS) flow_js.ml
	ocamlfind ocamlc $(INCLUDES) -package js_of_ocaml -o flow_js.cmo $(SYSLIBS) -c $^

flow.js: $(LIBS) $(OBJS) flow_js.cmo
	ocamlfind ocamlc -ccopt -O3 $(INCLUDES) -package js_of_ocaml -linkpkg -o flow_js.bc $(SYSLIBS) $^
	js_of_ocaml -opt 3 -o $@ flow_js.bc

test_files: $(LIBS) $(OPTOBJS) native_test_files.ml
	ocamlopt $(INCLUDES) -o test/native_test_files $^

clean::
	rm -f flow.js lexer_flow.ml
	$(MAKE) -C tools clean

depend::
	$(MAKE) -C tools depend
