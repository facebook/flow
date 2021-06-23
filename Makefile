# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.

################################################################################
#                            Variables to override                             #
################################################################################

EXTRA_INCLUDE_PATHS=
EXTRA_LIB_PATHS=
EXTRA_LIBS=
INTERNAL_MODULES=\
	src/hack_forked/stubs/logging/common\
	src/stubs
INTERNAL_NATIVE_C_FILES=
INTERNAL_BUILD_FLAGS=
INTERNAL_FLAGS=

ifeq ($(OS), Windows_NT)
  UNAME_S=Windows
  UNAME_M=
  SWITCH=ocaml-variants.4.09.1+mingw64c
  CC:=x86_64-w64-mingw32-gcc
  CXX:=x86_64-w64-mingw32-g++
  AR:=x86_64-w64-mingw32-gcc-ar
else
  UNAME_S=$(shell uname -s)
  UNAME_M=$(shell uname -m)
  SWITCH=ocaml-base-compiler.4.09.1
endif

# Default to `ocamlbuild -j 0` (unlimited parallelism), but you can limit it
# with `make OCAMLBUILD_JOBS=1`
OCAMLBUILD_JOBS := 0

-include facebook/Makefile.defs

################################################################################
#                              OS-dependent stuff                              #
################################################################################

ifeq ($(UNAME_S), Linux)
  EXTRA_LIBS += rt atomic
  INOTIFY=src/hack_forked/third-party/inotify
  INOTIFY_STUBS=$(INOTIFY)/inotify_stubs.c
  FSNOTIFY=src/hack_forked/fsnotify_linux
  FSNOTIFY_STUBS=
  FRAMEWORKS=
  EXE=
endif
ifeq ($(UNAME_S), FreeBSD)
  EXTRA_INCLUDE_PATHS += /usr/local/include
  EXTRA_LIB_PATHS += /usr/local/lib
  EXTRA_LIBS += inotify
  INOTIFY=src/hack_forked/third-party/inotify
  INOTIFY_STUBS=$(INOTIFY)/inotify_stubs.c
  FSNOTIFY=src/hack_forked/fsnotify_linux
  FSNOTIFY_STUBS=
  FRAMEWORKS=
  EXE=
endif
ifeq ($(UNAME_S), Darwin)
  INOTIFY=src/hack_forked/fsevents
  INOTIFY_STUBS=$(INOTIFY)/fsevents_stubs.c
  FSNOTIFY=src/hack_forked/fsnotify_darwin
  FSNOTIFY_STUBS=
  FRAMEWORKS=CoreServices CoreFoundation
  EXE=
endif
ifeq ($(UNAME_S), Windows)
  EXTRA_LIBS += atomic
  INOTIFY=
  INOTIFY_STUBS=
  FSNOTIFY=src/hack_forked/fsnotify_win
  FSNOTIFY_STUBS=$(FSNOTIFY)/fsnotify_stubs.c
  FRAMEWORKS=
  EXE=.exe
endif

################################################################################
#                                 Definitions                                  #
################################################################################

MODULES=\
  src/codemods\
  src/codemods/utils\
  src/commands\
  src/commands/config\
  src/commands/glean\
  src/commands/options\
  src/common\
  src/common/audit\
  src/common/build_id\
  src/common/cycle_hash\
  src/common/errors\
  src/common/exit\
  src/common/exit_status\
  src/common/leb128\
  src/common/lints\
  src/common/logging_utils\
  src/common/lwt\
  src/common/modulename\
  src/common/monad\
  src/common/packed_locs\
  src/common/profiling\
  src/common/semver\
  src/common/span\
  src/common/tarjan\
  src/common/transaction\
  src/common/ty\
  src/common/utils\
  src/common/utils/checked_set\
  src/common/utils/filename_cache\
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
  src/parser_utils/iloc\
  src/parser_utils/exports\
  src/parser_utils/output\
  src/parser_utils/output/printers\
  src/parser_utils/signature_builder\
  src/parser_utils/type_sig\
  src/parsing\
  src/procs\
  src/server\
  src/server/command_handler\
  src/server/env\
  src/server/error_collator\
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
  src/services/code_action\
  src/services/coverage\
  src/services/export\
  src/services/export/index\
  src/services/export/search\
  src/services/get_def\
  src/services/inference\
  src/services/inference/types\
  src/services/jsdoc\
  src/services/module\
  src/services/refactor\
  src/services/references\
  src/services/saved_state\
  src/services/saved_state/compression\
  src/services/saved_state/fetcher\
  src/services/type_info\
  src/state/heaps/context\
  src/state/heaps/diffing\
  src/state/heaps/module\
  src/state/heaps/package\
  src/state/heaps/parsing\
  src/state/heaps/parsing/exceptions\
  src/state/locals/module\
  src/state/readers\
  src/third-party/fuzzy-path/src\
  src/third-party/lz4\
  src/third-party/ocaml-sourcemaps/src\
  src/third-party/ocaml-vlq/src\
  src/typing\
  src/typing/coverage_response\
  src/typing/errors\
  src/typing/generics\
  src/typing/polarity\
  src/hack_forked/dfind\
  src/hack_forked/find\
  src/hack_forked/globals\
  src/heap\
  src/hack_forked/injection/default_injector\
  src/hack_forked/procs\
  src/hack_forked/search\
  src/hack_forked/socket\
  src/hack_forked/third-party/avl\
  src/hack_forked/third-party/core\
  src/hack_forked/utils/cgroup\
  src/hack_forked/utils/core\
  src/hack_forked/utils/buffered_line_reader\
  src/hack_forked/utils/build_mode/prod\
  src/hack_forked/utils/collections\
  src/hack_forked/utils/disk\
  src/hack_forked/utils/file_content\
  src/hack_forked/utils/file_url\
  src/hack_forked/utils/hh_json\
  src/hack_forked/utils/http_lite\
  src/hack_forked/utils/jsonrpc\
  src/hack_forked/utils/lsp\
  src/hack_forked/utils/marshal_tools\
  src/hack_forked/utils/opaque_digest\
  src/hack_forked/utils/procfs\
  src/hack_forked/utils/string\
  src/hack_forked/utils/sys\
  src/hack_forked/watchman\
  $(INOTIFY)\
  $(FSNOTIFY)\
  $(INTERNAL_MODULES)

LZ4_C_FILES=\
  $(sort $(wildcard src/third-party/lz4/*.c))

NATIVE_C_FILES=\
  $(INOTIFY_STUBS)\
  $(FSNOTIFY_STUBS)\
  src/common/xx/xx_stubs.c\
  src/services/saved_state/compression/saved_state_compression_stubs.c\
  src/hack_forked/find/hh_readdir.c\
  src/heap/hh_shared.c\
  src/hack_forked/utils/core/get_build_id.c\
  src/hack_forked/utils/sys/files.c\
  src/hack_forked/utils/sys/gc_profiling.c\
  src/hack_forked/utils/sys/getrusage.c\
  src/hack_forked/utils/sys/handle_stubs.c\
  src/hack_forked/utils/sys/nproc.c\
  src/hack_forked/utils/sys/priorities.c\
  src/hack_forked/utils/sys/processor_info.c\
  src/hack_forked/utils/sys/realpath.c\
  src/hack_forked/utils/sys/sysinfo.c\
  src/third-party/fuzzy-path/src/fuzzy_path_stubs.c\
  $(LZ4_C_FILES)\
  $(INTERNAL_NATIVE_C_FILES)

FINDLIB_PACKAGES=\
  base\
  bigarray\
  dtoa\
  lwt_ppx\
  lwt\
  ppx_let\
  sedlex\
  str

NATIVE_FINDLIB_PACKAGES=\
  $(FINDLIB_PACKAGES)\
  lwt_log\
  lwt.unix\
  unix

NATIVE_LIBRARIES=\
  stdc++\
  pthread\
  $(EXTRA_LIBS)

# On Windows, the linker (flexlink) handles -lfoo by searching for "libfoo",
# then "libfoo.dll.a" (dynamic linking), then "libfoo.a". Since we don't want
# users to have to install mingw64, we can either package the DLLs or
# statically link them. We choose to statically link them to maintain a single
# binary, by passing `-lfoo.a` which finds `libfoo.a` before looking (in vain)
# for `libfoo.a.dll.a`.
ifeq ($(UNAME_S), Windows)
NATIVE_LIBRARIES:=$(addsuffix .a,$(NATIVE_LIBRARIES))
endif

COPIED_FLOWLIB=\
	$(foreach lib,$(wildcard lib/*.js),_build/$(lib))

COPIED_PRELUDE=\
	$(foreach lib,$(wildcard prelude/*.js),_build/$(lib))

FINDLIB_JS_STUBS=$(shell ocamlfind query $(FINDLIB_PACKAGES) -predicates javascript -o-format -r)
JS_STUBS=\
	$(FINDLIB_JS_STUBS)\
	$(wildcard js/*.js)

OUNIT_TESTS=\
	src/common/lwt/__tests__/lwt_tests.native\
	src/common/ty/__tests__/ty_tests.native\
	src/common/utils/__tests__/common_utils_tests.native\
	src/common/semver/__tests__/semver_tests.native\
	src/parser/__tests__/parser_tests.native\
	src/parser_utils/__tests__/parser_utils_tests.native\
	src/parser_utils/output/__tests__/parser_utils_output_tests.native\
	src/parser_utils/output/printers/__tests__/parser_utils_output_printers_tests.native\
	src/services/references/__tests__/find_refs_tests.native\
	src/third-party/fuzzy-path/test/test.native
	# src/typing/__tests__/typing_tests.native

################################################################################
#                                    Rules                                     #
################################################################################

NATIVE_C_DIRS=$(patsubst %/,%,$(sort $(dir $(NATIVE_C_FILES))))
ALL_HEADER_FILES=$(addprefix _build/,$(shell find $(NATIVE_C_DIRS) -name '*.h'))
ALL_HEADER_FILES+=_build/src/third-party/lz4/xxhash.c
NATIVE_OBJECT_FILES=$(patsubst %.c,%.o,$(NATIVE_C_FILES))
NATIVE_OBJECT_FILES+=src/hack_forked/utils/core/get_build_id.gen.o
LZ4_OBJECT_FILES=$(patsubst %.c,%.o,$(LZ4_C_FILES))
BUILT_C_DIRS=$(addprefix _build/,$(NATIVE_C_DIRS))
BUILT_C_FILES=$(addprefix _build/,$(NATIVE_C_FILES))
BUILT_OBJECT_FILES=$(addprefix _build/,$(NATIVE_OBJECT_FILES))
BUILT_OUNIT_TESTS=$(addprefix _build/,$(OUNIT_TESTS))
BUILT_LZ4_OBJECT_FILES=$(addprefix _build/,$(LZ4_OBJECT_FILES))

FUZZY_PATH_DEPS=src/third-party/fuzzy-path/libfuzzy-path.a
FUZZY_PATH_LINKER_FLAGS=$(FUZZY_PATH_DEPS)
LIBFUZZY_PATH_DEP=_build/src/third-party/fuzzy-path/.depends-on-libfuzzy-path

# Any additional C flags can be added here
ifeq ($(UNAME_M), aarch64)
  CC_FLAGS=
else
  CC_FLAGS=-mcx16
endif
CC_FLAGS += $(EXTRA_CC_FLAGS)
CC_OPTS=$(foreach flag, $(CC_FLAGS), -ccopt $(flag))
INCLUDE_OPTS=$(foreach dir,$(MODULES),-I $(dir))
JS_FINDLIB_OPTS=$(foreach lib,$(FINDLIB_PACKAGES),-pkg $(lib))
NATIVE_FINDLIB_OPTS=$(foreach lib,$(NATIVE_FINDLIB_PACKAGES),-pkg $(lib))
NATIVE_LIB_OPTS=$(foreach lib, $(NATIVE_LIBRARIES),-cclib -l -cclib $(lib))
ALL_INCLUDE_PATHS=$(sort $(realpath $(BUILT_C_DIRS))) $(EXTRA_INCLUDE_PATHS)
EXTRA_INCLUDE_OPTS=$(foreach dir, $(ALL_INCLUDE_PATHS),-ccopt -I -ccopt $(dir))
EXTRA_LIB_OPTS=$(foreach dir, $(EXTRA_LIB_PATHS),-cclib -L -cclib $(dir))
FRAMEWORK_OPTS=$(foreach framework, $(FRAMEWORKS),-cclib -framework -cclib $(framework))

BYTECODE_LINKER_FLAGS=$(NATIVE_OBJECT_FILES) $(FUZZY_PATH_LINKER_FLAGS) $(NATIVE_LIB_OPTS) $(EXTRA_LIB_OPTS) $(FRAMEWORK_OPTS)
LINKER_FLAGS=$(BYTECODE_LINKER_FLAGS)

# For fuzzy-path
CXXFLAGS=-s -std=c++11 -Wall -O3 -static-libstdc++

RELEASE_TAGS=$(if $(FLOW_RELEASE),-tag warn_a,)

OCB=ocamlbuild -use-ocamlfind -no-links -j $(OCAMLBUILD_JOBS)

all: bin/flow$(EXE)

all-homebrew:
	export OPAMROOT="$(shell mktemp -d 2> /dev/null || mktemp -d -t opam)"; \
	export OPAMYES="1"; \
	export FLOW_RELEASE="1"; \
	opam init --bare --no-setup --disable-sandboxing && \
	rm -rf _opam && \
	opam switch create . --deps-only $(SWITCH) && \
	opam exec -- make

.PHONY: deps
deps:
	[ -d _opam ] || opam switch create . $(SWITCH) --deps-only --yes

clean:
	if command -v ocamlbuild >/dev/null; then ocamlbuild -clean; fi
	rm -rf _build
	rm -rf bin
	rm -f src/hack_forked/utils/core/get_build_id.gen.c
	rm -f flow.odocl

build-flow: _build/scripts/ppx_gen_flowlibs.exe $(LIBFUZZY_PATH_DEP) $(BUILT_OBJECT_FILES) $(COPIED_FLOWLIB) $(COPIED_PRELUDE) $(INTERNAL_BUILD_FLAGS)
	$(OCB) $(INTERNAL_FLAGS) $(INCLUDE_OPTS) -tag thread $(NATIVE_FINDLIB_OPTS) \
		-lflags "$(LINKER_FLAGS)" \
		$(RELEASE_TAGS) \
		src/flow.native

build-flow-debug: _build/scripts/ppx_gen_flowlibs.exe $(LIBFUZZY_PATH_DEP) $(BUILT_OBJECT_FILES) $(COPIED_FLOWLIB) $(COPIED_PRELUDE) $(INTERNAL_BUILD_FLAGS)
	$(OCB) $(INTERNAL_FLAGS) $(INCLUDE_OPTS) -tag thread $(NATIVE_FINDLIB_OPTS) \
		-lflags -custom -lflags "$(LINKER_FLAGS)" \
		src/flow.d.byte
	mkdir -p bin
	cp _build/src/flow.d.byte bin/flow$(EXE)

testgen: build-flow
	$(OCB) $(INTERNAL_FLAGS) $(INCLUDE_OPTS) -tag thread $(NATIVE_FINDLIB_OPTS) \
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

src/hack_forked/utils/core/get_build_id.gen.c: FORCE scripts/script_utils.ml scripts/gen_build_id.ml
	ocaml -safe-string -I scripts -w -3 unix.cma scripts/gen_build_id.ml $@

_build/src/hack_forked/utils/core/get_build_id.gen.c: FORCE scripts/script_utils.ml scripts/gen_build_id.ml
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
	$(OCB) -I scripts -I src/common/xx -tag linkall -pkg unix scripts/ppx_gen_flowlibs/ppx_gen_flowlibs.cmxa

_build/scripts/ppx_gen_flowlibs/ppx_gen_flowlibs_standalone.cmxa: scripts/ppx_gen_flowlibs/ppx_gen_flowlibs_standalone.ml
	$(OCB) -I scripts -tag linkall -pkg unix scripts/ppx_gen_flowlibs/ppx_gen_flowlibs_standalone.cmxa

_build/scripts/ppx_gen_flowlibs.exe: $(BUILT_LZ4_OBJECT_FILES) _build/src/common/xx/xx_stubs.o _build/scripts/ppx_gen_flowlibs/ppx_gen_flowlibs.cmxa _build/scripts/ppx_gen_flowlibs/ppx_gen_flowlibs_standalone.cmxa
	ocamlfind ocamlopt -linkpkg -linkall \
		-package ppxlib,unix \
		-I _build/scripts/ppx_gen_flowlibs \
		-ccopt "$(BUILT_LZ4_OBJECT_FILES) _build/src/common/xx/xx_stubs.o" \
		_build/scripts/ppx_gen_flowlibs/ppx_gen_flowlibs.cmxa \
		_build/scripts/ppx_gen_flowlibs/ppx_gen_flowlibs_standalone.cmxa \
		-o "$@"

_build/src/third-party/fuzzy-path _build/src/third-party/fuzzy-path/src _build/src/third-party/fuzzy-path/vendor:
	mkdir -p $@

_build/src/third-party/fuzzy-path/src/%.o: src/third-party/fuzzy-path/src/%.cpp | _build/src/third-party/fuzzy-path/src
	$(CXX) $(CXXFLAGS) -o $@ -c $<

_build/src/third-party/fuzzy-path/vendor/%.o: src/third-party/fuzzy-path/vendor/%.cpp | _build/src/third-party/fuzzy-path/vendor
	$(CXX) $(CXXFLAGS) -o $@ -c $<

_build/src/third-party/fuzzy-path/libfuzzy-path.a: _build/src/third-party/fuzzy-path/src/fuzzy_path_wrapper.o _build/src/third-party/fuzzy-path/vendor/MatcherBase.o _build/src/third-party/fuzzy-path/vendor/score_match.o | _build/src/third-party/fuzzy-path
	$(AR) $(ARFLAGS) $@ $^

# ocamlbuild doesn't invalidate its caches when prebuilt libraries change, so we have to
# delete artifacts to force it to recompile when libfuzzy-path.a changes
# https://github.com/ocaml/ocamlbuild/issues/221
$(LIBFUZZY_PATH_DEP): _build/src/third-party/fuzzy-path/libfuzzy-path.a
	rm -f _build/src/third-party/fuzzy-path/src/fuzzy_path_stubs.o
	rm -f _build/src/flow.native
	rm -f _build/src/flow.d.byte
	rm -f _build/src/flow_dot_js.byte
	rm -f _build/src/third-party/fuzzy-path/test/test.native
	touch "$@"

_build/src/third-party/fuzzy-path/src/fuzzy_path_stubs.o: $(LIBFUZZY_PATH_DEP)

_build/src/third-party/fuzzy-path/test/test.native: $(LIBFUZZY_PATH_DEP)

bin/flow$(EXE): build-flow
	mkdir -p $(@D)
	cp _build/src/flow.native $@

# builds each ounit test individually
$(BUILT_OUNIT_TESTS): $(BUILT_OBJECT_FILES) FORCE
	$(OCB) $(INTERNAL_FLAGS) $(INCLUDE_OPTS) -tag thread $(NATIVE_FINDLIB_OPTS) \
		-I $(patsubst _build/%,%,$(@D)) \
		-lflags "$(LINKER_FLAGS)" \
		$(patsubst _build/%,%,$@)

# builds all ounit tests at once
.PHONY: build-ounit-tests
build-ounit-tests: $(BUILT_OBJECT_FILES) FORCE
	$(OCB) $(INTERNAL_FLAGS) $(INCLUDE_OPTS) -tag thread $(NATIVE_FINDLIB_OPTS) \
		$(foreach dir,$(dir $(OUNIT_TESTS)),-I $(dir)) \
		-lflags "$(LINKER_FLAGS)" \
		$(OUNIT_TESTS)

.PHONY: ounit-tests
ounit-tests: build-ounit-tests
	@for cmd in $(BUILT_OUNIT_TESTS); do \
		echo "Running $$cmd:"; \
		"$$cmd"; \
	done

.PHONY: ounit-tests-ci
ounit-tests-ci: build-ounit-tests
	mkdir -p test-results/ounit
	for cmd in $(OUNIT_TESTS); do \
		"_build/$$cmd" -output-junit-file "test-results/ounit/$${cmd//\//zS}.xml"; \
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

js: _build/scripts/ppx_gen_flowlibs.exe $(LIBFUZZY_PATH_DEP) $(BUILT_OBJECT_FILES) $(COPIED_FLOWLIB) $(COPIED_PRELUDE)
	mkdir -p bin
	$(OCB) \
		-pkg js_of_ocaml \
		-build-dir _build \
		-lflags -custom \
		$(INCLUDE_OPTS) $(JS_FINDLIB_OPTS) \
		-lflags "$(BYTECODE_LINKER_FLAGS)" \
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
