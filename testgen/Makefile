# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.

DIR:=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))
TOP=$(DIR)/..
PARSER_DIR=src/parser
TESTGEN_DIR=testgen

OCB = ocamlbuild -use-ocamlfind -pkgs sedlex -I src/third-party/wtf8

NATIVE_OBJECT_FILES=\
	hack/utils/files.o\
	hack/utils/nproc.o\
	hack/utils/realpath.o\
	hack/utils/sysinfo.o\
	hack/utils/processor_info.o\
	hack/utils/priorities.o

RUNNER_DEPS=\
	hack/heap\
	hack/injection/default_injector\
	hack/utils\
	hack/utils/collections\
	hack/utils/disk\
	hack/utils/hh_json\
	hack/third-party/core\
	src/common\
	src/common/utils\
	src/common/span\
	src/common/lints\
	src/common/errors\
	src/common/profiling\
	src/stubs\
	src/parser\
	src/commands\
	src/commands/config\
	src/flowlib\
	src/typing\
	src/parser_utils

all: testgen

clean:
	$(OCB) -clean; \
	cd $(TOP); \
	$(OCB) -clean; \
	rm -f $(PARSER_DIR)/flow_parser.js $(PARSER_DIR)/tools/native_test_files; \
	rm -fr $(TESTGEN_DIR)/node_modules; \
	rm -f $(TESTGEN_DIR)/flowtestgen.json; \
	rm -f $(TESTGEN_DIR)/.flowconfig; \
	rm -f $(TESTGEN_DIR)/.babelrc; \
	rm -f $(TESTGEN_DIR)/package-lock.json; \
	rm -f $(TESTGEN_DIR)/package.json

build-parser:
	cd $(TOP); \
	$(OCB) -no-links $(PARSER_DIR)/parser_flow.cmxa

testgen: build-parser 
	cd $(TOP); \
	$(OCB) \
		-ocamlc "ocamlopt -ccopt -DNO_SQLITE3" \
		$(NATIVE_OBJECT_FILES); \
	$(OCB) \
		$(foreach dir,$(RUNNER_DEPS),-I $(dir)) \
	 	-lib unix -lib str \
	 	-lflags "$(NATIVE_OBJECT_FILES)" \
		-tag debug \
		$(TESTGEN_DIR)/flowtestgen.native; \
	pwd;\
	rm -f $(TESTGEN_DIR)/flowtestgen.native; \
	mv -f $(TOP)/flowtestgen.native $(TESTGEN_DIR)/flowtestgen.native
