# Copyright (c) 2014, Facebook, Inc.
# All rights reserved.

all: build-parser

clean:
	ocamlbuild -clean
	rm -f flow_parser.js tools/native_test_files

build-parser: 
	ocamlbuild -no-links parser_flow.cmxa

parserjs: 
	ocamlbuild -use-ocamlfind -pkgs js_of_ocaml flow_parser_js.byte
	js_of_ocaml -opt 3 -o flow_parser.js flow_parser_js.byte
	rm flow_parser_js.byte

test: build-parser parserjs 
	./test/run_esprima_tests.js
	./test/run_hardcoded_tests.js

tools: native_test_files

native_test_files:
	ocamlbuild tools/native_test_files.native
	mv native_test_files.native tools/native_test_files
