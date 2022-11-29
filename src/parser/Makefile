# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

DIR:=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))
TOP=$(DIR)/../..
REL_DIR=src/parser
DUNE_OUT="$(TOP)/_build/default/$(REL_DIR)"

OCAML_PATH=$(shell ocamlc -where)
OCAML_HEADERS=\
	$(sort $(patsubst $(OCAML_PATH)/%,dist/libflowparser/include/%, \
		$(filter $(OCAML_PATH)/%,$(shell \
			$(CC) -I $(OCAML_PATH) -MM -MT deps libflowparser.h \
	))))

all:

clean:
	rm -f flow_parser.js
	rm -rf dist

.PHONY: libflowparser.exe.o
libflowparser.exe.o:
	dune build --profile opt ./libflowparser.exe.o

dist/libflowparser/lib/libflowparser.a: libflowparser.exe.o libflowparser.h
	@mkdir -p "$(@D)"
	if [ ! -e "$@" -o "$(DUNE_OUT)/libflowparser.exe.o" -nt "$@" -o libflowparser.h -nt "$@" ]; then \
		ar rcs "$@" "$(DUNE_OUT)/libflowparser.exe.o"; \
	fi

$(OCAML_HEADERS): dist/libflowparser/include/%: $(OCAML_PATH)/%
	@mkdir -p "$(@D)"
	cp "$<" "$@"

dist/libflowparser/include/flowparser/libflowparser.h: libflowparser.h
	@mkdir -p "$(@D)"
	cp "$<" "$@"

dist/libflowparser.zip: \
		$(OCAML_HEADERS) \
		dist/libflowparser/include/flowparser/libflowparser.h \
		dist/libflowparser/lib/libflowparser.a
	cd dist && zip -r $(@F) libflowparser

js:
	dune build --no-print-directory --profile opt flow_parser_dot_js.bc.js
	@if [ ! -e flow_parser.js -o "$(TOP)/_build/default/$(REL_DIR)/flow_parser_dot_js.bc.js" -nt flow_parser.js ]; then \
		cp "$(TOP)/_build/default/$(REL_DIR)/flow_parser_dot_js.bc.js" flow_parser.js; \
		chmod +w flow_parser.js; \
	fi

test-js: js
	cd $(TOP)/packages/flow-parser; npm test

test-esprima-ocaml:
	dune build @parser_esprima_tests

test-hardcoded-ocaml:
	dune build @parser_flow_tests

test-ocaml: test-esprima-ocaml test-hardcoded-ocaml

test: test-js test-ocaml
