# Copyright (c) 2013-present, Facebook, Inc.
# All rights reserved.

################################################################################
#                            Variables to override                             #
################################################################################

EXTRA_INCLUDE_PATHS=
EXTRA_LIB_PATHS=
INTERNAL_MODULES=hack/stubs src/stubs
INTERNAL_NATIVE_C_FILES=

ifeq ($(OS), Windows_NT)
  UNAME_S=Windows
else
  UNAME_S=$(shell uname -s)
endif

-include facebook/Makefile

################################################################################
#                              OS-dependent stuff                              #
################################################################################

ifeq ($(UNAME_S), Linux)
  INOTIFY=hack/third-party/inotify
  INOTIFY_STUBS=$(INOTIFY)/inotify_stubs.c
  FSNOTIFY=hack/fsnotify_linux
  FSNOTIFY_STUBS=
  RT=rt
  FRAMEWORKS=
  EXE=
endif
ifeq ($(UNAME_S), Darwin)
  INOTIFY=hack/fsevents
  INOTIFY_STUBS=$(INOTIFY)/fsevents_stubs.c
  FSNOTIFY=hack/fsnotify_darwin
  FSNOTIFY_STUBS=
  RT=
  FRAMEWORKS=CoreServices CoreFoundation
  EXE=
endif
ifeq ($(UNAME_S), Windows)
  INOTIFY=
  INOTIFY_STUBS=
  FSNOTIFY=hack/fsnotify_win
  FSNOTIFY_STUBS=$(FSNOTIFY)/fsnotify_stubs.c
  RT=
  FRAMEWORKS=
  EXE=.exe
endif

################################################################################
#                                 Definitions                                  #
################################################################################

MODULES=\
  src/commands\
  src/commands/config\
  src/common\
  src/common/audit\
  src/common/errors\
  src/common/lints\
  src/common/profiling\
  src/common/utils\
  src/dts\
  src/flowlib\
  src/parser\
  src/parser_utils\
  src/parsing\
  src/server\
  src/services/autocomplete\
  src/services/inference\
  src/services/flowFileGen\
  src/services/port\
  src/services/type_info\
  src/third-party/lz4\
  src/typing\
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
  hack/utils\
  hack/utils/collections\
  hack/utils/disk\
  hack/utils/hh_json\
  $(INOTIFY)\
  $(FSNOTIFY)\
  $(INTERNAL_MODULES)

NATIVE_C_FILES=\
  $(INOTIFY_STUBS)\
  $(FSNOTIFY_STUBS)\
  hack/heap/hh_shared.c\
  hack/utils/files.c\
  hack/utils/get_build_id.c\
  hack/utils/handle_stubs.c\
  hack/utils/nproc.c\
  hack/utils/realpath.c\
  hack/utils/sysinfo.c\
  hack/utils/priorities.c\
  $(sort $(wildcard src/third-party/lz4/*.c))\
  $(INTERNAL_NATIVE_C_FILES)

OCAML_LIBRARIES=\
  unix\
  str\
  bigarray

NATIVE_LIBRARIES=\
  pthread\
  $(RT)

OCP_BUILD_FILES=\
  ocp_build_flow.ocp\
  ocp_build_hack.ocp

COPIED_FLOWLIB=\
	$(foreach lib,$(wildcard lib/*.js),_build/$(lib))

JS_STUBS=\
	$(wildcard js/*.js)


# We need caml_hexstring_of_float for js_of_ocaml < 2.8
JSOO_VERSION=$(shell which js_of_ocaml 2> /dev/null > /dev/null && js_of_ocaml --version)
JSOO_MAJOR=$(shell echo $(JSOO_VERSION) | cut -d. -f 1)
JSOO_MINOR=$(shell echo $(JSOO_VERSION) | cut -d. -f 2)
ifeq (1, $(shell [[ -z "$(JSOO_VERSION)" ]] || [ $(JSOO_MAJOR) -gt 2 ] || [ $(JSOO_MAJOR) -eq 2 -a $(JSOO_MINOR) -gt 7 ]; echo $$?))
	JS_STUBS += js/optional/caml_hexstring_of_float.js
endif

################################################################################
#                                    Rules                                     #
################################################################################

NATIVE_C_DIRS=$(patsubst %/,%,$(sort $(dir $(NATIVE_C_FILES))))
ALL_HEADER_FILES=$(addprefix _build/,$(shell find $(NATIVE_C_DIRS) -name '*.h'))
NATIVE_OBJECT_FILES=$(patsubst %.c,%.o,$(NATIVE_C_FILES))
NATIVE_OBJECT_FILES+=hack/utils/get_build_id.gen.o
BUILT_C_DIRS=$(addprefix _build/,$(NATIVE_C_DIRS))
BUILT_C_FILES=$(addprefix _build/,$(NATIVE_C_FILES))
BUILT_OBJECT_FILES=$(addprefix _build/,$(NATIVE_OBJECT_FILES))

CC_FLAGS=-DNO_SQLITE3
CC_FLAGS += $(EXTRA_CC_FLAGS)
CC_OPTS=$(foreach flag, $(CC_FLAGS), -ccopt $(flag))
INCLUDE_OPTS=$(foreach dir,$(MODULES),-I $(dir))
LIB_OPTS=$(foreach lib,$(OCAML_LIBRARIES),-lib $(lib))
NATIVE_LIB_OPTS=$(foreach lib, $(NATIVE_LIBRARIES),-cclib -l -cclib $(lib))
ALL_INCLUDE_PATHS=$(sort $(realpath $(BUILT_C_DIRS))) $(EXTRA_INCLUDE_PATHS)
EXTRA_INCLUDE_OPTS=$(foreach dir, $(ALL_INCLUDE_PATHS),-ccopt -I -ccopt $(dir))
EXTRA_LIB_OPTS=$(foreach dir, $(EXTRA_LIB_PATHS),-cclib -L -cclib $(dir))
FRAMEWORK_OPTS=$(foreach framework, $(FRAMEWORKS),-cclib -framework -cclib $(framework))

BYTECODE_LINKER_FLAGS=$(NATIVE_OBJECT_FILES) $(NATIVE_LIB_OPTS) $(EXTRA_LIB_OPTS) $(FRAMEWORK_OPTS)
LINKER_FLAGS=$(BYTECODE_LINKER_FLAGS)

all: build-flow copy-flow-files
all-ocp: build-flow-with-ocp copy-flow-files-ocp

all-homebrew:
	export OPAMROOT="$(shell mktemp -d 2> /dev/null || mktemp -d -t opam)"; \
	export OPAMYES="1"; \
	opam init --no-setup && \
	opam pin add flowtype . && \
	opam install flowtype --deps-only && \
	opam config exec -- make

clean:
	ocamlbuild -clean
	rm -rf bin
	rm -f hack/utils/get_build_id.gen.c
	rm -f flow.odocl

clean-ocp: clean
	[ -d _obuild ] && ocp-build clean || true
	rm -f $(OCP_BUILD_FILES)

build-flow: _build/scripts/ppx_gen_flowlibs.native $(BUILT_OBJECT_FILES) $(COPIED_FLOWLIB)
	ocamlbuild \
		-use-ocamlfind -pkgs sedlex \
		-no-links  $(INCLUDE_OPTS) $(LIB_OPTS) \
		-lflags "$(LINKER_FLAGS)" \
		src/flow.native

%.ocp: %.ocp.fb scripts/utils.ml scripts/ocp_build_glob.ml
	ocaml -I scripts -w -3 str.cma unix.cma scripts/ocp_build_glob.ml $(addsuffix .fb,$@) $@

build-flow-with-ocp: $(OCP_BUILD_FILES) hack/utils/get_build_id.gen.c
	[ -d _obuild ] || ocp-build init
	 # Force us to pick up libdef changes - ocp-build is fast so it's fine
	rm -rf _obuild/flow-flowlib
	ocp-build build flow
	mkdir -p bin
	cp _obuild/flow/flow.asm$(EXE) bin/flow$(EXE)
	rm -f $(OCP_BUILD_FILES)

build-parser-test-with-ocp: $(OCP_BUILD_FILES) hack/utils/get_build_id.gen.c
	[ -d _obuild ] || ocp-build init
	ocp-build build flow-parser-hardcoded-test
	rm -f $(OCP_BUILD_FILES)

test-parser-ocp: $(OCP_BUILD_FILES) hack/utils/get_build_id.gen.c
	[ -d _obuild ] || ocp-build init
	ocp-build tests flow-parser-hardcoded-test
	rm -f $(OCP_BUILD_FILES)

build-flow-debug: _build/scripts/ppx_gen_flowlibs.native $(BUILT_OBJECT_FILES) $(COPIED_FLOWLIB)
	ocamlbuild -lflags -custom -no-links $(INCLUDE_OPTS) $(LIB_OPTS) -lflags "$(LINKER_FLAGS)" src/flow.d.byte
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

hack/utils/get_build_id.gen.c: FORCE scripts/utils.ml scripts/gen_build_id.ml
	ocaml -I scripts -w -3 unix.cma scripts/gen_build_id.ml $@

_build/hack/utils/get_build_id.gen.c: FORCE scripts/utils.ml scripts/gen_build_id.ml
	ocaml -I scripts -w -3 unix.cma scripts/gen_build_id.ml $@

$(COPIED_FLOWLIB): _build/%.js: %.js
	mkdir -p $(dir $@)
	cp $< $@
	rm -rf _build/src/flowlib

_build/scripts/ppx_gen_flowlibs.native: scripts/ppx_gen_flowlibs.ml
	ocamlbuild \
		-use-ocamlfind -pkgs sedlex,compiler-libs.common,unix \
		-I scripts \
		scripts/ppx_gen_flowlibs.native
	rm -f ppx_gen_flowlibs.native

copy-flow-files: build-flow
	mkdir -p bin
	cp _build/src/flow.native bin/flow$(EXE)

copy-flow-files-ocp: build-flow-with-ocp
	mkdir -p bin
	cp _obuild/flow/flow.asm bin/flow$(EXE)

do-test:
	./runtests.sh bin/flow$(EXE)
	bin/flow$(EXE) check
	./tool test

test: build-flow copy-flow-files
	${MAKE} do-test

test-ocp: build-flow-with-ocp copy-flow-files-ocp
	${MAKE} do-test

js: $(BUILT_OBJECT_FILES)
	mkdir -p bin
	# NOTE: temporarily disabling warning 31 because
	# hack/third-party/core/result.ml and the opam `result` module both define
	# result.cma, and this is the most expedient (though fragile) way to unblock
	# ourselves.
	ocamlbuild -use-ocamlfind \
		-pkgs js_of_ocaml,sedlex \
		-build-dir _build \
		-lflags -custom -no-links \
		$(INCLUDE_OPTS) $(LIB_OPTS) \
		-lflags "$(BYTECODE_LINKER_FLAGS) -warn-error -31" \
		src/flow_dot_js.byte
	# js_of_ocaml has no ability to upgrade warnings to errors, but we want to
	# error if, for example, there are any unimplemented C primitives.
	js_of_ocaml \
			--opt 3 \
			--disable genprim \
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

FORCE:

.PHONY: all js build-flow build-flow-with-ocp build-flow-debug FORCE

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
		| grep -v "src/parser/ast.ml" \
		| grep -v "src/dts/dts_ast.ml" \
		| sed "s/\.ml$$//" > $@
	rm -f deps last_deps temp_deps

flow.docdir/index.html: flow.odocl
	ocamlbuild $(INCLUDE_OPTS) -use-ocamlfind flow.docdir/index.html

doc: flow.docdir/index.html
