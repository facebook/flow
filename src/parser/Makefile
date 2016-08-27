# Copyright (c) 2013-present, Facebook, Inc.
# All rights reserved.

DIR:=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))
TOP=$(DIR)/../..
REL_DIR=src/parser

JSOO_VERSION=$(shell which js_of_ocaml 2> /dev/null > /dev/null && js_of_ocaml --version)
JSOO_MAJOR=$(shell echo $(JSOO_VERSION) | cut -d. -f 1)
JSOO_MINOR=$(shell echo $(JSOO_VERSION) | cut -d. -f 2)

NATIVE_OBJECT_FILES=\
	hack/utils/files.o\
	hack/utils/nproc.o\
	hack/utils/realpath.o\
	hack/utils/sysinfo.o\
	hack/utils/priorities.o\
	hack/utils/win32_support.o

RUNNER_DEPS=\
	hack/heap\
	hack/injection/default_injector\
	hack/utils\
	hack/utils/collections\
	hack/utils/disk\
	hack/utils/hh_json\
	hack/third-party/core\
	src/common\
	src/parser

all: build-parser

clean:
	cd $(TOP); \
	ocamlbuild -clean; \
	rm -f $(REL_DIR)/flow_parser.js $(REL_DIR)/tools/native_test_files

build-parser:
	cd $(TOP); \
	ocamlbuild -no-links $(REL_DIR)/parser_flow.cmxa

js:
	cd $(TOP); \
	ocamlbuild -use-ocamlfind -pkgs js_of_ocaml $(REL_DIR)/flow_parser_js.byte; \
	\
	if [ $(JSOO_MAJOR) -gt 2 ] || [ $(JSOO_MAJOR) -eq 2 -a $(JSOO_MINOR) -gt 7 ]; then \
	    js_of_ocaml --opt 3 -o $(REL_DIR)/flow_parser.js _build/$(REL_DIR)/flow_parser_js.byte; \
	else \
	    js_of_ocaml --opt 3 -o $(REL_DIR)/flow_parser.js $(TOP)/js/optional/caml_hexstring_of_float.js _build/$(REL_DIR)/flow_parser_js.byte; \
	fi; \
	\
	rm flow_parser_js.byte

test-js: build-parser js
	npm test

../../_build/$(REL_DIR)/test/run_hardcoded_tests.native: build-parser
	cd $(TOP); \
	ocamlbuild \
		-ocamlc "ocamlopt" \
		$(NATIVE_OBJECT_FILES); \
	ocamlbuild \
		$(foreach dir,$(RUNNER_DEPS),-I $(dir)) \
	 	-lib unix -lib str \
	 	-lflags "$(NATIVE_OBJECT_FILES)" \
		$(REL_DIR)/test/run_hardcoded_tests.native; \
	rm run_hardcoded_tests.native

test-ocaml: ../../_build/$(REL_DIR)/test/run_hardcoded_tests.native
	cd $(TOP); \
	_build/$(REL_DIR)/test/run_hardcoded_tests.native $(REL_DIR)/test/hardcoded_tests.js

test: test-js test-ocaml

tools: native_test_files

native_test_files:
	cd $(TOP); \
	ocamlbuild $(REL_DIR)/tools/native_test_files.native; \
	mv native_test_files.native $(REL_DIR)/tools/native_test_files

npm-publish:
	mv README.md README-backup.md
	mv README-npm.md README.md
	-npm publish
	mv README.md README-npm.md
	mv README-backup.md README.md
