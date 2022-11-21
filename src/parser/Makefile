# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

DIR:=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))
TOP=$(DIR)/../..
REL_DIR=src/parser

OCB = ocamlbuild -use-ocamlfind -I src/third-party/sedlex -I src/hack_forked/utils/collections

NATIVE_OBJECT_FILES=\
	src/hack_forked/utils/sys/gc_profiling.o\
	src/hack_forked/utils/sys/nproc.o\
	src/hack_forked/utils/sys/sysinfo.o\
	src/hack_forked/utils/sys/processor_info.o

RUNNER_DEPS=\
	src/heap\
	src/hack_forked/injection/default_injector\
	src/hack_forked/utils/collections\
	src/hack_forked/utils/core\
	src/hack_forked/utils/disk\
	src/hack_forked/utils/hh_json\
	src/hack_forked/utils/string\
	src/hack_forked/utils/sys\
	src/common/utils\
	src/parser\
	src/parser_utils

OCAML_PATH=$(shell ocamlc -where)
OCAML_HEADERS=\
	$(sort $(patsubst $(OCAML_PATH)/%,dist/libflowparser/include/%, \
		$(filter $(OCAML_PATH)/%,$(shell \
			$(CC) -I $(OCAML_PATH) -MM -MT deps libflowparser.h \
	))))

all:

clean:
	$(OCB) -clean; \
	cd $(TOP); \
	$(OCB) -clean; \
	rm -f $(REL_DIR)/flow_parser.js; \
	rm -rf $(REL_DIR)/dist

.PHONY: libflowparser.native.o
libflowparser.native.o:
	cd $(TOP) && $(OCB) -no-links -tag "runtime_variant(_pic)" \
		$(REL_DIR)/libflowparser.native.o

dist/libflowparser/lib/libflowparser.a: libflowparser.native.o
	@mkdir -p "$(@D)"
	if [ ! -e "$@" -o "$(OCAML_PATH)/libasmrun_pic.a" -nt "$@" -o  "$(TOP)/_build/$(REL_DIR)/libflowparser.native.o" -nt "$@" ]; then \
		echo "Rebuilding $@"; \
		cp "$(OCAML_PATH)/libasmrun_pic.a" "$@"; \
		ar rcs "$@" "$(TOP)/_build/$(REL_DIR)/libflowparser.native.o"; \
	else \
		echo "Not rebuilding $@, already up to date"; \
	fi
	test -e "$@" || exit 1

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
