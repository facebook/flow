# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

################################################################################
#                            Variables to override                             #
################################################################################

EXTRA_INCLUDE_PATHS=
EXTRA_LIB_PATHS=
EXTRA_LIBS=
INTERNAL_NATIVE_LIBS=
INTERNAL_MODULES=\
	src/hack_forked/stubs/logging/common\
	src/stubs
INTERNAL_NATIVE_C_FILES=
INTERNAL_BUILD_FLAGS=
INTERNAL_FLAGS=

ifeq ($(OS), Windows_NT)
  UNAME_S=Windows
  UNAME_M=
  SWITCH=ocaml-variants.4.14.0+mingw64c
  CC:=x86_64-w64-mingw32-gcc
  CXX:=x86_64-w64-mingw32-g++
  AR:=x86_64-w64-mingw32-gcc-ar
else
  UNAME_S=$(shell uname -s)
  UNAME_M=$(shell uname -m)
  SWITCH=ocaml-base-compiler.4.14.0
endif

JS_OF_OCAML_VERSION=4.0.0

# Default to `ocamlbuild -j 0` (unlimited parallelism), but you can limit it
# with `make OCAMLBUILD_JOBS=1`
OCAMLBUILD_JOBS := 0

-include facebook/Makefile.defs

################################################################################
#                              OS-dependent stuff                              #
################################################################################

ifeq ($(UNAME_S), Linux)
  EXTRA_LIBS += rt
  INOTIFY=src/third-party/inotify
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
  INOTIFY=src/third-party/inotify
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
  src/analysis\
  src/analysis/env_builder\
  src/codemods\
  src/codemods/utils\
  src/commands\
  src/commands/config\
  src/commands/extra\
  src/commands/glean\
  src/commands/options\
  src/common\
  src/common/audit\
  src/common/build_id\
  src/common/cas_digest\
  src/common/cycle_hash\
  src/common/dirent\
  src/common/errors\
  src/common/exit\
  src/common/exit_status\
  src/common/leb128\
  src/common/lints\
  src/common/logging_utils\
  src/common/lwt\
  src/common/lz4\
  src/common/modulename\
  src/common/monad\
  src/common/packed_locs\
  src/common/profiling\
  src/common/semver\
  src/common/span\
  src/common/tarjan\
  src/common/transaction\
  src/common/ty\
  src/common/utils/checked_set\
  src/common/utils/filename_cache\
  src/common/utils/loc_utils\
  src/common/utils\
  src/common/vcs\
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
  src/services/references\
  src/services/saved_state\
  src/services/saved_state/compression\
  src/services/saved_state/fetcher\
  src/services/type_info\
  src/state/heaps/context\
  src/state/heaps/diffing\
  src/state/heaps/module\
  src/state/heaps/parsing\
  src/state/heaps/parsing/exceptions\
  src/state/locals/module\
  src/state/readers\
  src/third-party/sedlex\
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
  src/common/dirent/dirent_stubs.c\
  src/common/lz4/lz4_stubs.c\
  src/common/xx/xx_stubs.c\
  src/services/saved_state/compression/saved_state_compression_stubs.c\
  src/hack_forked/find/hh_readdir.c\
  src/heap/hh_shared.c\
  src/hack_forked/utils/core/get_build_id.c\
  src/hack_forked/utils/core/fast_compare.c\
  src/hack_forked/utils/sys/gc_profiling.c\
  src/hack_forked/utils/sys/handle_stubs.c\
  src/hack_forked/utils/sys/nproc.c\
  src/hack_forked/utils/sys/processor_info.c\
  src/hack_forked/utils/sys/sysinfo.c\
  src/parser_utils/type_sig/type_sig_bin_stubs.c\
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
  $(EXTRA_LIBS)\
  $(INTERNAL_NATIVE_LIBS)

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
NATIVE_FINDLIB_OPTS=$(foreach lib,$(NATIVE_FINDLIB_PACKAGES),-pkg $(lib))
NATIVE_LIB_OPTS=$(foreach lib, $(NATIVE_LIBRARIES),-cclib -l -cclib $(lib))
ALL_INCLUDE_PATHS=$(sort $(realpath $(BUILT_C_DIRS))) $(EXTRA_INCLUDE_PATHS)
EXTRA_INCLUDE_OPTS=$(foreach dir, $(ALL_INCLUDE_PATHS),-ccopt -I -ccopt $(dir))
EXTRA_LIB_OPTS=$(foreach dir, $(EXTRA_LIB_PATHS),-cclib -L -cclib $(dir))
FRAMEWORK_OPTS=$(foreach framework, $(FRAMEWORKS),-cclib -framework -cclib $(framework))

LINKER_FLAGS=$(NATIVE_OBJECT_FILES) $(FUZZY_PATH_LINKER_FLAGS) $(NATIVE_LIB_OPTS) $(EXTRA_LIB_OPTS) $(FRAMEWORK_OPTS)

# For fuzzy-path
CXXFLAGS=-s -std=c++11 -Wall -O3 -static-libstdc++

RELEASE_TAGS=$(if $(FLOW_RELEASE),-tag warn_a,)

OCB=ocamlbuild -use-ocamlfind -no-links -j $(OCAMLBUILD_JOBS)

NPM?=npm
YARN?=yarn

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

.PHONY: deps-js
deps-js:
	opam install js_of_ocaml.$(JS_OF_OCAML_VERSION)

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

.PHONY: ounit-tests
ounit-tests:
	dune runtest

.PHONY: ounit-tests-ci
ounit-tests-ci:
	mkdir -p test-results/ounit
	OUNIT_CI=true OUNIT_OUTPUT_JUNIT_FILE='$(shell pwd)/test-results/ounit/$$(suite_name).xml' dune runtest --force

do-test-js: bin/flow.js
	node src/__tests__/flow_dot_js_smoke_test.js $(realpath bin/flow.js)

do-test:
	./runtests.sh bin/flow$(EXE)
	bin/flow$(EXE) check packages/flow-dev-tools
	${MAKE} do-test-tool
	./tool test

do-test-tool:
	FLOW_BIN=../../bin/flow$(EXE) ${YARN} --cwd packages/flow-dev-tools test

test-tool: bin/flow$(EXE)
	${MAKE} do-test-tool

test-js: bin/flow.js do-test-js

test: bin/flow$(EXE) bin/flow.js
	${MAKE} do-test do-test-js

.PHONY: bin/flow.js
bin/flow.js:
	@mkdir -p $(@D)
	dune build --profile opt src/flow_dot_js.bc.js
	install -C _build/default/src/flow_dot_js.bc.js "$@"

js: bin/flow.js

dist/flow/flow$(EXE): build-flow
	mkdir -p $(@D)
	cp _build/src/flow.native $@

dist/flow.zip: dist/flow/flow$(EXE)
	cd dist && zip -r $(@F) flow/flow$(EXE)

dist/npm-%.tgz: FORCE
	@mkdir -p $(@D)
	@mkdir -p npm-$(*F)-tmp
	cd npm-$(*F)-tmp && ${NPM} pack ../packages/$(*F)/
	mv npm-$(*F)-tmp/$(*F)-*.tgz dist/npm-$(*F).tgz
	@rm -rf npm-$(*F)-tmp

FORCE:

.PHONY: all js build-flow build-flow-debug FORCE

# Don't run in parallel because of https://github.com/ocaml/ocamlbuild/issues/300
.NOTPARALLEL:

-include facebook/Makefile

print-switch:
	@echo $(SWITCH)

print-jsoo-version:
	@echo $(JS_OF_OCAML_VERSION)
