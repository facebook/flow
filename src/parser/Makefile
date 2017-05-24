# Copyright (c) 2013-present, Facebook, Inc.
# All rights reserved.

DIR:=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))
TOP=$(DIR)/../..
REL_DIR=src/parser

JSOO_VERSION=$(shell which js_of_ocaml 2> /dev/null > /dev/null && js_of_ocaml --version)
JSOO_MAJOR=$(shell echo $(JSOO_VERSION) | cut -d. -f 1)
JSOO_MINOR=$(shell echo $(JSOO_VERSION) | cut -d. -f 2)

OCB = ocamlbuild -use-ocamlfind

NATIVE_OBJECT_FILES=\
	hack/utils/files.o\
	hack/utils/nproc.o\
	hack/utils/realpath.o\
	hack/utils/sysinfo.o\
	hack/utils/priorities.o

RUNNER_DEPS=\
	hack/heap\
	hack/injection/default_injector\
	hack/utils\
	hack/utils/collections\
	hack/utils/disk\
	hack/utils/hh_json\
	hack/third-party/core\
	src/common/utils\
	src/parser\
	src/parser_utils

all: build-parser

clean:
	$(OCB) -clean; \
	cd $(TOP); \
	$(OCB) -clean; \
	rm -f $(REL_DIR)/flow_parser.js $(REL_DIR)/tools/native_test_files

build-parser:
	cd $(TOP); \
	$(OCB) -no-links $(REL_DIR)/parser_flow.cmxa

js:
	cd $(TOP); \
	$(OCB) -pkgs js_of_ocaml $(REL_DIR)/flow_parser_dot_js.byte; \
	\
	if [ $(JSOO_MAJOR) -gt 2 ] || [ $(JSOO_MAJOR) -eq 2 -a $(JSOO_MINOR) -gt 7 ]; then \
	    js_of_ocaml --opt 3 \
				-o $(REL_DIR)/flow_parser.js \
				_build/$(REL_DIR)/flow_parser_dot_js.byte; \
	else \
	    js_of_ocaml --opt 3 \
				-o $(REL_DIR)/flow_parser.js \
				$(TOP)/js/optional/caml_hexstring_of_float.js \
				_build/$(REL_DIR)/flow_parser_dot_js.byte; \
	fi; \
	\
	rm flow_parser_dot_js.byte

test-js: build-parser js
	npm test

../../_build/$(REL_DIR)/test/run_esprima_tests.native: build-parser
	cd $(TOP); \
	$(OCB) \
		-ocamlc "ocamlopt" \
		$(NATIVE_OBJECT_FILES); \
	$(OCB) \
		$(foreach dir,$(RUNNER_DEPS),-I $(dir)) \
	 	-lib unix -lib str \
	 	-lflags "$(NATIVE_OBJECT_FILES)" \
		$(REL_DIR)/test/run_esprima_tests.native; \
	rm run_esprima_tests.native

../../_build/$(REL_DIR)/test/run_hardcoded_tests.native: build-parser
	cd $(TOP); \
	$(OCB) \
		-ocamlc "ocamlopt -ccopt -DNO_SQLITE3" \
		$(NATIVE_OBJECT_FILES); \
	$(OCB) \
		$(foreach dir,$(RUNNER_DEPS),-I $(dir)) \
	 	-lib unix -lib str \
	 	-lflags "$(NATIVE_OBJECT_FILES)" \
		-tag debug \
		$(REL_DIR)/test/run_hardcoded_tests.native; \
	rm run_hardcoded_tests.native

test-esprima-ocaml: ../../_build/$(REL_DIR)/test/run_esprima_tests.native
	cd $(TOP); \
	_build/$(REL_DIR)/test/run_esprima_tests.native $(REL_DIR)/test/esprima/

test-hardcoded-ocaml: ../../_build/$(REL_DIR)/test/run_esprima_tests.native
	cd $(TOP); \
	_build/$(REL_DIR)/test/run_esprima_tests.native $(REL_DIR)/test/flow/

test-ocaml: test-esprima-ocaml test-hardcoded-ocaml

test: test-js test-ocaml

tools: native_test_files

native_test_files:
	cd $(TOP); \
	$(OCB) $(REL_DIR)/tools/native_test_files.native; \
	mv native_test_files.native $(REL_DIR)/tools/native_test_files

npm-pack:
	mv README.md README-backup.md
	mv README-npm.md README.md
	-npm pack
	mv README.md README-npm.md
	mv README-backup.md README.md

npm-publish:
	mv README.md README-backup.md
	mv README-npm.md README.md
	-npm publish
	mv README.md README-npm.md
	mv README-backup.md README.md

ocamlfind-install:
	$(OCB) parser_flow.cma parser_flow.cmxa
	ocamlfind install flow_parser META \
		_build/parser_flow.a \
		_build/parser_flow.cma \
		_build/parser_flow.cmxa \
		_build/*.cmi

lexer.native: lexer.ml
	$(OCB) $@

tools/native_test_files: ../../_build/$(REL_DIR)/tools/native_test_files

../../_build/$(REL_DIR)/tools/native_test_files: build-parser
	cd $(TOP); \
	$(OCB) -ocamlc "ocamlopt" $(NATIVE_OBJECT_FILES); \
	$(OCB) \
		-I $(REL_DIR) \
	 	-lib unix -lib str \
	 	-lflags "$(NATIVE_OBJECT_FILES)" \
		$(REL_DIR)/tools/native_test_files.native; \
	rm native_test_files.native; \
	mv _build/$(REL_DIR)/tools/native_test_files.native $(REL_DIR)/tools/native_test_files
