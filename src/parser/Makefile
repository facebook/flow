# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

DIR:=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))
TOP=$(DIR)/../..
REL_DIR=src/parser
DUNE_OUT="$(TOP)/_build/default/$(REL_DIR)"

OCAML_PATH=$(shell ocamlc -where)

all:

clean:
	rm -f flow_parser.js
	rm -rf dist

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
