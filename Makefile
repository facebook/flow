# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.

################################################################################
#                            Variables to override                             #
################################################################################

EXTRA_INCLUDE_PATHS=
EXTRA_LIB_PATHS=
EXTRA_LIBS=
INTERNAL_MODULES=\
	hack/stubs/logging\
	hack/stubs/logging/common\
	src/stubs
INTERNAL_NATIVE_C_FILES=
INTERNAL_BUILD_FLAGS=
INTERNAL_FLAGS=

ifeq ($(OS), Windows_NT)
  UNAME_S=Windows
else
  UNAME_S=$(shell uname -s)
endif

-include facebook/Makefile.defs

################################################################################
#                              OS-dependent stuff                              #
################################################################################

ifeq ($(UNAME_S), Linux)
  EXTRA_LIBS += rt
  INOTIFY=hack/third-party/inotify
  INOTIFY_STUBS=$(INOTIFY)/inotify_stubs.c
  FSNOTIFY=hack/fsnotify_linux
  FSNOTIFY_STUBS=
  FRAMEWORKS=
  EXE=
endif
ifeq ($(UNAME_S), FreeBSD)
  EXTRA_INCLUDE_PATHS += /usr/local/include
  EXTRA_LIB_PATHS += /usr/local/lib
  EXTRA_LIBS += inotify
  INOTIFY=hack/third-party/inotify
  INOTIFY_STUBS=$(INOTIFY)/inotify_stubs.c
  FSNOTIFY=hack/fsnotify_linux
  FSNOTIFY_STUBS=
  FRAMEWORKS=
  EXE=
endif
ifeq ($(UNAME_S), Darwin)
  INOTIFY=hack/fsevents
  INOTIFY_STUBS=$(INOTIFY)/fsevents_stubs.c
  FSNOTIFY=hack/fsnotify_darwin
  FSNOTIFY_STUBS=
  FRAMEWORKS=CoreServices CoreFoundation
  EXE=
endif
ifeq ($(UNAME_S), Windows)
  INOTIFY=
  INOTIFY_STUBS=
  FSNOTIFY=hack/fsnotify_win
  FSNOTIFY_STUBS=$(FSNOTIFY)/fsnotify_stubs.c
  FRAMEWORKS=
  EXE=.exe
endif

################################################################################
#                                 Definitions                                  #
################################################################################

MODULES=\
  src/commands\
  src/commands/config\
  src/commands/options\
  src/common\
  src/common/audit\
  src/common/build_id\
  src/common/errors\
  src/common/exit_status\
  src/common/lints\
  src/common/logging_utils\
  src/common/lwt\
  src/common/modulename\
  src/common/monad\
  src/common/profiling\
  src/common/semver\
  src/common/span\
  src/common/tarjan\
  src/common/transaction\
  src/common/ty\
  src/common/utils\
  src/common/utils/loc_utils\
  src/common/xx\
  src/flowlib\
  src/lsp\
  src/monitor\
  src/monitor/connections\
  src/monitor/logger\
  src/monitor/rpc\
  src/monitor/status\
  src/monitor/utils\
  src/parser\
  src/parser_utils\
  src/parser_utils/aloc\
  src/parser_utils/output\
  src/parser_utils/output/printers\
  src/parsing\
  src/procs\
  src/server\
  src/server/command_handler\
  src/server/env\
  src/server/error_collator\
  src/server/find_refs\
  src/server/lazy_mode_utils\
  src/server/monitor_listener\
  src/server/persistent_connection\
  src/server/protocol\
  src/server/rechecker\
  src/server/server_files\
  src/server/server_utils\
  src/server/shmem\
  src/server/watchman_expression_terms\
  src/services/autocomplete\
  src/services/coverage\
  src/services/get_def\
  src/services/inference\
  src/services/inference/module\
  src/services/flowFileGen\
  src/services/saved_state\
  src/services/type_info\
  src/state/heaps/context\
  src/state/heaps/module\
  src/state/heaps/parsing\
  src/state/heaps/parsing/exceptions\
  src/state/locals/module\
  src/state/readers\
  src/third-party/lz4\
  src/third-party/ocaml-sourcemaps/src\
  src/third-party/ocaml-vlq/src\
  src/typing\
  src/typing/coverage_response\
  src/typing/errors\
  src/typing/polarity\
  hack/dfind\
  hack/find\
  hack/globals\
  hack/heap\
  hack/injection/default_injector\
  hack/procs\
  hack/search\
  hack/socket\
  hack/third-party/avl\
  hack/third-party/core\
  hack/utils/cgroup\
  hack/utils/core\
  hack/utils/buffered_line_reader\
  hack/utils/build_mode/prod\
  hack/utils/collections\
  hack/utils/disk\
  hack/utils/file_content\
  hack/utils/file_url\
  hack/utils/hh_json\
  hack/utils/http_lite\
  hack/utils/jsonrpc\
  hack/utils/lsp\
  hack/utils/marshal_tools\
  hack/utils/opaque_digest\
  hack/utils/procfs\
  hack/utils/string\
  hack/utils/sys\
  hack/watchman\
  $(INOTIFY)\
  $(FSNOTIFY)\
  $(INTERNAL_MODULES)

NATIVE_C_FILES=\
  $(INOTIFY_STUBS)\
  $(FSNOTIFY_STUBS)\
  src/common/xx/xx_stubs.c\
  src/services/saved_state/saved_state_compression_stubs.c\
  hack/heap/hh_assert.c\
  hack/heap/hh_shared.c\
  hack/utils/core/get_build_id.c\
  hack/utils/sys/files.c\
  hack/utils/sys/gc_profiling.c\
  hack/utils/sys/getrusage.c\
  hack/utils/sys/handle_stubs.c\
  hack/utils/sys/nproc.c\
  hack/utils/sys/priorities.c\
  hack/utils/sys/processor_info.c\
  hack/utils/sys/realpath.c\
  hack/utils/sys/sysinfo.c\
  $(sort $(wildcard src/third-party/lz4/*.c))\
  $(INTERNAL_NATIVE_C_FILES)

FINDLIB_PACKAGES=\
  sedlex\
  lwt\
  lwt_log\
  lwt.unix\
  lwt_ppx\
  unix\
  str\
  bigarray\
	ppx_let

NATIVE_LIBRARIES=\
  pthread\
  $(EXTRA_LIBS)

COPIED_FLOWLIB=\
	$(foreach lib,$(wildcard lib/*.js),_build/$(lib))

COPIED_PRELUDE=\
	$(foreach lib,$(wildcard prelude/*.js),_build/$(lib))

JS_STUBS=\
	+dtoa/dtoa_stubs.js\
	$(wildcard js/*.js)

OUNIT_TESTS=\
	src/commands/config/__tests__/command_config_tests.native\
	src/common/lwt/__tests__/lwt_tests.native\
	src/common/ty/__tests__/ty_tests.native\
	src/common/utils/__tests__/common_utils_tests.native\
	src/common/semver/__tests__/semver_tests.native\
	src/parser/__tests__/parser_tests.native\
	src/parser_utils/__tests__/parser_utils_tests.native\
	src/parser_utils/output/__tests__/parser_utils_output_tests.native\
	src/parser_utils/output/printers/__tests__/parser_utils_output_printers_tests.native\
	src/server/find_refs/__tests__/find_refs_tests.native
	# src/typing/__tests__/typing_tests.native

################################################################################
#                                    Rules                                     #
################################################################################

NATIVE_C_DIRS=$(patsubst %/,%,$(sort $(dir $(NATIVE_C_FILES))))
ALL_HEADER_FILES=$(addprefix _build/,$(shell find $(NATIVE_C_DIRS) -name '*.h'))
ALL_HEADER_FILES+=_build/src/third-party/lz4/xxhash.c
NATIVE_OBJECT_FILES=$(patsubst %.c,%.o,$(NATIVE_C_FILES))
NATIVE_OBJECT_FILES+=hack/utils/core/get_build_id.gen.o
BUILT_C_DIRS=$(addprefix _build/,$(NATIVE_C_DIRS))
BUILT_C_FILES=$(addprefix _build/,$(NATIVE_C_FILES))
BUILT_OBJECT_FILES=$(addprefix _build/,$(NATIVE_OBJECT_FILES))
BUILT_OUNIT_TESTS=$(addprefix _build/,$(OUNIT_TESTS))

CC_FLAGS=-DNO_SQLITE3
CC_FLAGS += $(EXTRA_CC_FLAGS)
CC_OPTS=$(foreach flag, $(CC_FLAGS), -ccopt $(flag))
INCLUDE_OPTS=$(foreach dir,$(MODULES),-I $(dir))
FINDLIB_OPTS=$(foreach lib,$(FINDLIB_PACKAGES),-pkg $(lib))
NATIVE_LIB_OPTS=$(foreach lib, $(NATIVE_LIBRARIES),-cclib -l -cclib $(lib))
ALL_INCLUDE_PATHS=$(sort $(realpath $(BUILT_C_DIRS))) $(EXTRA_INCLUDE_PATHS)
EXTRA_INCLUDE_OPTS=$(foreach dir, $(ALL_INCLUDE_PATHS),-ccopt -I -ccopt $(dir))
EXTRA_LIB_OPTS=$(foreach dir, $(EXTRA_LIB_PATHS),-cclib -L -cclib $(dir))
FRAMEWORK_OPTS=$(foreach framework, $(FRAMEWORKS),-cclib -framework -cclib $(framework))

BYTECODE_LINKER_FLAGS=$(NATIVE_OBJECT_FILES) $(NATIVE_LIB_OPTS) $(EXTRA_LIB_OPTS) $(FRAMEWORK_OPTS)
LINKER_FLAGS=$(BYTECODE_LINKER_FLAGS)

RELEASE_TAGS=$(if $(FLOW_RELEASE),-tag warn_a,)

OCB=ocamlbuild -use-ocamlfind -no-links

all: bin/flow$(EXE)

all-homebrew:
	export OPAMROOT="$(shell mktemp -d 2> /dev/null || mktemp -d -t opam)"; \
	export OPAMYES="1"; \
	export FLOW_RELEASE="1"; \
	opam init --bare --no-setup --disable-sandboxing && \
	rm -rf _opam && \
	opam switch create . --deps-only && \
	opam exec -- make

clean:
	ocamlbuild -clean
	rm -rf bin
	rm -f hack/utils/core/get_build_id.gen.c
	rm -f flow.odocl

build-flow: _build/scripts/ppx_gen_flowlibs.exe $(BUILT_OBJECT_FILES) $(COPIED_FLOWLIB) $(COPIED_PRELUDE) $(INTERNAL_BUILD_FLAGS)
	# Both lwt and lwt_ppx provide ppx stuff. Fixed in lwt 4.0.0
	# https://github.com/ocsigen/lwt/issues/453
	export OCAMLFIND_IGNORE_DUPS_IN="$(shell ocamlfind query lwt)"; \
	$(OCB) $(INTERNAL_FLAGS) $(INCLUDE_OPTS) $(FINDLIB_OPTS) \
		-lflags "$(LINKER_FLAGS)" \
		$(RELEASE_TAGS) \
		src/flow.native

build-flow-debug: _build/scripts/ppx_gen_flowlibs.exe $(BUILT_OBJECT_FILES) $(COPIED_FLOWLIB) $(COPIED_PRELUDE) $(INTERNAL_BUILD_FLAGS)
	$(OCB) $(INTERNAL_FLAGS) $(INCLUDE_OPTS) $(FINDLIB_OPTS) \
		-lflags -custom -lflags "$(LINKER_FLAGS)" \
		src/flow.d.byte
	mkdir -p bin
	cp _build/src/flow.d.byte bin/flow$(EXE)

testgen: build-flow
	$(OCB) $(INTERNAL_FLAGS) $(INCLUDE_OPTS) $(FINDLIB_OPTS) \
	 	-lflags "$(LINKER_FLAGS)" \
		$(RELEASE_TAGS) \
		testgen/flowtestgen.native
	mkdir -p bin
	cp _build/testgen/flowtestgen.native bin/flowtestgen$(EXE)

%.h: $(subst _build/,,$@)
	mkdir -p $(dir $@)
	cp $(subst _build/,,$@) $@

# Compile each object file. Equivalent to this ocamlbuild rule, but faster:
# ocamlbuild -ocamlc "ocamlopt $(EXTRA_INCLUDE_OPTS) $(CC_OPTS)" $(subst _build/,,$@)
$(BUILT_C_FILES): _build/%.c: %.c
	mkdir -p $(dir $@)
	cp $< $@
$(BUILT_OBJECT_FILES): %.o: %.c $(ALL_HEADER_FILES)
	cd $(dir $@) && ocamlopt $(EXTRA_INCLUDE_OPTS) $(CC_OPTS) -c $(notdir $<)

hack/utils/core/get_build_id.gen.c: FORCE scripts/script_utils.ml scripts/gen_build_id.ml
	ocaml -safe-string -I scripts -w -3 unix.cma scripts/gen_build_id.ml $@

_build/hack/utils/core/get_build_id.gen.c: FORCE scripts/script_utils.ml scripts/gen_build_id.ml
	ocaml -safe-string -I scripts -w -3 unix.cma scripts/gen_build_id.ml $@

$(COPIED_FLOWLIB): _build/%.js: %.js
	mkdir -p $(dir $@)
	cp $< $@
	rm -rf _build/src/flowlib

$(COPIED_PRELUDE): _build/%.js: %.js
	mkdir -p $(dir $@)
	cp $< $@
	rm -rf _build/src/prelude

_build/scripts/ppx_gen_flowlibs/ppx_gen_flowlibs.cmxa: scripts/script_utils.ml scripts/ppx_gen_flowlibs/ppx_gen_flowlibs.ml
	$(OCB) -I scripts -tag linkall -pkg unix scripts/ppx_gen_flowlibs/ppx_gen_flowlibs.cmxa

_build/scripts/ppx_gen_flowlibs/ppx_gen_flowlibs_standalone.cmxa: scripts/ppx_gen_flowlibs/ppx_gen_flowlibs_standalone.ml
	$(OCB) -I scripts -tag linkall -pkg unix scripts/ppx_gen_flowlibs/ppx_gen_flowlibs_standalone.cmxa

_build/scripts/ppx_gen_flowlibs.exe: _build/scripts/ppx_gen_flowlibs/ppx_gen_flowlibs.cmxa _build/scripts/ppx_gen_flowlibs/ppx_gen_flowlibs_standalone.cmxa
	ocamlfind ocamlopt -linkpkg -linkall \
		-package ocaml-migrate-parsetree,unix \
		-I _build/scripts/ppx_gen_flowlibs \
		_build/scripts/ppx_gen_flowlibs/ppx_gen_flowlibs.cmxa \
		_build/scripts/ppx_gen_flowlibs/ppx_gen_flowlibs_standalone.cmxa \
		-o "$@"

bin/flow$(EXE): build-flow
	mkdir -p $(@D)
	cp _build/src/flow.native $@

$(BUILT_OUNIT_TESTS): $(BUILT_OBJECT_FILES) FORCE
	export OCAMLFIND_IGNORE_DUPS_IN="$(shell ocamlfind query lwt)"; \
	$(OCB) $(INTERNAL_FLAGS) $(INCLUDE_OPTS) $(FINDLIB_OPTS) \
		-I $(patsubst _build/%,%,$(@D)) \
		-lflags "$(LINKER_FLAGS)" \
		$(patsubst _build/%,%,$@)

.PHONY: build-ounit-tests
build-ounit-tests: $(BUILT_OBJECT_FILES) FORCE
	export OCAMLFIND_IGNORE_DUPS_IN="$(shell ocamlfind query lwt)"; \
	$(OCB) $(INTERNAL_FLAGS) $(INCLUDE_OPTS) $(FINDLIB_OPTS) \
		$(foreach dir,$(dir $(OUNIT_TESTS)),-I $(dir)) \
		-lflags "$(LINKER_FLAGS)" \
		$(OUNIT_TESTS)

.PHONY: ounit-tests
ounit-tests: build-ounit-tests
	@for cmd in $(BUILT_OUNIT_TESTS); do \
		echo "Running $$cmd:"; \
		"$$cmd"; \
	done

do-test:
	./runtests.sh bin/flow$(EXE)
	bin/flow$(EXE) check packages/flow-dev-tools
	${MAKE} do-test-tool
	./tool test

do-test-tool:
	FLOW_BIN=../../bin/flow$(EXE) ${MAKE} -C packages/flow-dev-tools test

test-tool: bin/flow$(EXE)
	${MAKE} do-test-tool

test: bin/flow$(EXE)
	${MAKE} do-test

js: _build/scripts/ppx_gen_flowlibs.exe $(BUILT_OBJECT_FILES) $(COPIED_FLOWLIB)
	mkdir -p bin
	# NOTE: temporarily disabling warning 31 because
	# hack/third-party/core/result.ml and the opam `result` module both define
	# result.cma, and this is the most expedient (though fragile) way to unblock
	# ourselves.
	$(OCB) \
		-pkg js_of_ocaml \
		-build-dir _build \
		-lflags -custom \
		$(INCLUDE_OPTS) $(FINDLIB_OPTS) \
		-lflags "$(BYTECODE_LINKER_FLAGS) -warn-error -31" \
		src/flow_dot_js.byte
	# js_of_ocaml has no ability to upgrade warnings to errors, but we want to
	# error if, for example, there are any unimplemented C primitives.
	js_of_ocaml \
			--opt 3 \
			--disable genprim \
			--extern-fs \
			-o bin/flow.js \
			$(JS_STUBS) _build/src/flow_dot_js.byte \
			2>_build/js_of_ocaml.err; \
	ret=$$?; \
	if [ ! $$ret ]; then \
		exit $$ret; \
	elif [ -s _build/js_of_ocaml.err ]; then \
		printf "js_of_ocaml produced output on stderr:\n" 1>&2; \
		cat _build/js_of_ocaml.err 1>&2; \
		exit 1; \
	fi

dist/flow/flow$(EXE): build-flow
	mkdir -p $(@D)
	cp _build/src/flow.native $@

dist/flow.zip: dist/flow/flow$(EXE)
	cd dist && zip -r $(@F) flow/flow$(EXE)

dist/npm-%.tgz: FORCE
	@mkdir -p $(@D)
	@mkdir -p npm-$(*F)-tmp
	cd npm-$(*F)-tmp && npm pack ../packages/$(*F)/
	mv npm-$(*F)-tmp/$(*F)-*.tgz dist/npm-$(*F).tgz
	@rm -rf npm-$(*F)-tmp

FORCE:

.PHONY: all js build-flow build-flow-debug FORCE

# Don't run in parallel because of https://github.com/ocaml/ocamlbuild/issues/300
.NOTPARALLEL:

# This rule runs if any .ml or .mli file has been touched. It recursively calls
# ocamldep to figure out all the modules that we use to build src/flow.ml
flow.odocl: $(shell find . -name "*.ml" -o -name "*.mli")
	echo "src/flow.ml" > deps
	echo "" > last_deps
	until diff deps last_deps > /dev/null; do \
		cp deps last_deps; \
		cat deps \
		  | xargs ocamldep -one-line $(INCLUDE_OPTS) \
		  | grep -o "[a-zA-Z0-9/_-]*\.cm[xo]" \
		  | sed "s/\.cm[xo]$$/.ml/" \
		  | sort -u > temp_deps; \
		mv temp_deps deps; \
	done
	# For some reason these two AST files cause ocamldoc to get stuck
	cat deps \
		| grep -v "src/parser/flow_ast.ml" \
		| sed "s/\.ml$$//" > $@
	rm -f deps last_deps temp_deps

flow.docdir/index.html: flow.odocl
	ocamlbuild $(INCLUDE_OPTS) -use-ocamlfind flow.docdir/index.html

doc: flow.docdir/index.html

-include facebook/Makefile
