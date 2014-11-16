TOP=../../../../hphp/hack/src/
SRC = native_test_files.ml

INCLUDEDIRS=..

LIBS=../parser_flow.cma

-include $(TOP)/Makefile.common

all:: native_test_files

native_test_files: native_test_files.cmx
	ocamlopt $(INCLUDES) -o $@ $(LIBS:.cma=.cmxa) $^

clean:: 
	rm -f native_test_files
