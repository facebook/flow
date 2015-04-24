# Copyright (c) 2014, Facebook, Inc.
# All rights reserved.

all: build-parser

clean:
	ocamlbuild -clean
	rm -f flow_parser.js tools/native_test_files

build-parser: 
	ocamlbuild -no-links parser_flow.cmxa

js:
	ocamlbuild -use-ocamlfind -pkgs js_of_ocaml -build-dir _build flow_parser_js.byte
	js_of_ocaml --opt 3 -o flow_parser.js _build/flow_parser_js.byte
	rm _build/flow_parser_js.byte

test: build-parser js
	npm test

tools: native_test_files

native_test_files:
	ocamlbuild tools/native_test_files.native
	mv native_test_files.native tools/native_test_files

npm-publish:
	mv README.md README-backup.md
	mv README-npm.md README.md
	-npm publish
	mv README.md README-npm.md
	mv README-backup.md README.md
