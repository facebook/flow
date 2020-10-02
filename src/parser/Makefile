# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

DIR:=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))
TOP=$(DIR)/../..
REL_DIR=src/parser

OCB = ocamlbuild -use-ocamlfind

NATIVE_OBJECT_FILES=\
	src/hack_forked/utils/sys/files.o\
	src/hack_forked/utils/sys/gc_profiling.o\
	src/hack_forked/utils/sys/getrusage.o\
	src/hack_forked/utils/sys/nproc.o\
	src/hack_forked/utils/sys/realpath.o\
	src/hack_forked/utils/sys/sysinfo.o\
	src/hack_forked/utils/sys/processor_info.o\
	src/hack_forked/utils/sys/priorities.o

RUNNER_DEPS=\
	src/heap\
	src/hack_forked/injection/default_injector\
	src/hack_forked/third-party/core\
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

all: build-parser

clean:
	$(OCB) -clean; \
	cd $(TOP); \
	$(OCB) -clean; \
	rm -f $(REL_DIR)/flow_parser.js; \
	rm -rf $(REL_DIR)/dist

build-parser:
	cd $(TOP); \
	$(OCB) -no-links $(REL_DIR)/parser_flow.cmxa

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
	cd $(TOP); \
	$(OCB) -no-links -pkgs js_of_ocaml $(REL_DIR)/flow_parser_dot_js.byte; \
	[ -e "$(REL_DIR)/flow_parser.js" -a "$(REL_DIR)/flow_parser.js" -nt "_build/$(REL_DIR)/flow_parser_dot_js.byte" ] || \
		js_of_ocaml --opt 3 \
			-o $(REL_DIR)/flow_parser.js \
			_build/$(REL_DIR)/flow_parser_dot_js.byte

test-js: js
	cd $(TOP)/packages/flow-parser; npm test

../../_build/$(REL_DIR)/test/run_tests.native: build-parser
	cd $(TOP); \
	$(OCB) -no-links \
		-ocamlc "ocamlopt" \
		$(NATIVE_OBJECT_FILES); \
	$(OCB) -no-links \
		$(foreach dir,$(RUNNER_DEPS),-I $(dir)) \
	 	-lflags "$(NATIVE_OBJECT_FILES)" \
		$(REL_DIR)/test/run_tests.native

test-esprima-ocaml: ../../_build/$(REL_DIR)/test/run_tests.native
	cd $(TOP); \
	_build/$(REL_DIR)/test/run_tests.native $(REL_DIR)/test/esprima/

test-hardcoded-ocaml: ../../_build/$(REL_DIR)/test/run_tests.native
	cd $(TOP); \
	_build/$(REL_DIR)/test/run_tests.native $(REL_DIR)/test/flow/

test-ocaml: test-esprima-ocaml test-hardcoded-ocaml

test: test-js test-ocaml

ocamlfind-install:
	cd $(TOP); \
	$(OCB) $(REL_DIR)/parser_flow.cma $(REL_DIR)/parser_flow.cmxa; \
	ocamlfind install flow_parser $(REL_DIR)/META \
		_build/$(REL_DIR)/parser_flow.a \
		_build/$(REL_DIR)/parser_flow.cma \
		_build/$(REL_DIR)/parser_flow.cmxa \
		_build/$(REL_DIR)/*.cmi

lexer.native: lexer.ml
	$(OCB) $@
