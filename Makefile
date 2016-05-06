# Copyright (c) 2013-present, Facebook, Inc.
# All rights reserved.

################################################################################
#                            Variables to override                             #
################################################################################

EXTRA_INCLUDE_PATHS=
EXTRA_LIB_PATHS=

################################################################################
#                              OS-dependent stuff                              #
################################################################################

OS=$(shell uname -s)

FLOWLIB=bin/flowlib.tar.gz

ifeq ($(OS), Linux)
  INOTIFY=third-party/inotify
  INOTIFY_STUBS=$(INOTIFY)/inotify_stubs.o
  FSNOTIFY=fsnotify_linux
  ELF=elf
  FRAMEWORKS=
  SECTCREATE=
endif
ifeq ($(OS), Darwin)
  INOTIFY=fsevents
  INOTIFY_STUBS=$(INOTIFY)/fsevents_stubs.o
  FSNOTIFY=fsnotify_darwin
  ELF=
  FRAMEWORKS=CoreServices CoreFoundation
  SECTCREATE=-cclib -sectcreate -cclib __text -cclib flowlib -cclib $(abspath $(FLOWLIB))
endif

################################################################################
#                                 Definitions                                  #
################################################################################

MODULES=\
  src/commands\
  src/common\
  src/dts\
  src/embedded\
  src/parser\
  src/parsing\
  src/server\
  src/services/autocomplete\
  src/services/inference\
  src/services/port\
  src/stubs\
  src/typing\
  hack/dfind\
  hack/find\
  hack/globals\
  hack/heap\
  hack/hhi\
  hack/procs\
  hack/search\
  hack/socket\
  hack/stubs\
  hack/third-party/avl\
  hack/third-party/core\
  hack/utils\
  hack/utils/collections\
  hack/utils/hh_json\
  hack/$(INOTIFY)\
  hack/$(FSNOTIFY)

NATIVE_OBJECT_FILES=\
  hack/$(INOTIFY_STUBS)\
  hack/heap/hh_shared.o\
  hack/hhi/hhi_elf.o\
  hack/utils/files.o\
  hack/utils/get_build_id.gen.o\
  hack/utils/get_build_id.o\
  hack/utils/handle_stubs.o\
  hack/utils/nproc.o\
  hack/utils/realpath.o\
  hack/utils/sysinfo.o\
  hack/utils/priorities.o\
  hack/utils/win32_support.o\
  hack/hhi/hhi_win32res_stubs.o\
  src/embedded/flowlib_elf.o

OCAML_LIBRARIES=\
  unix\
  str\
  bigarray

NATIVE_LIBRARIES=\
  pthread\
  $(ELF)

FILES_TO_COPY=\
  $(wildcard lib/*.js)

JS_STUBS=\
	$(wildcard js/*.js)

################################################################################
#                                    Rules                                     #
################################################################################

ALL_HEADER_FILES=$(addprefix _build/,$(shell find hack -name '*.h'))
BUILT_OBJECT_FILES=$(addprefix _build/,$(NATIVE_OBJECT_FILES))
BUILT_C_FILES=$(patsubst %.o,%.c,$(BUILT_OBJECT_FILES))

CC_FLAGS=-DNO_LZ4
CC_FLAGS += $(EXTRA_CC_FLAGS)
CC_OPTS=$(foreach flag, $(CC_FLAGS), -ccopt $(flag))
INCLUDE_OPTS=$(foreach dir,$(MODULES),-I $(dir))
LIB_OPTS=$(foreach lib,$(OCAML_LIBRARIES),-lib $(lib))
NATIVE_LIB_OPTS=$(foreach lib, $(NATIVE_LIBRARIES),-cclib -l -cclib $(lib))
EXTRA_INCLUDE_OPTS=$(foreach dir, $(EXTRA_INCLUDE_PATHS),-ccopt -I -ccopt $(dir))
EXTRA_LIB_OPTS=$(foreach dir, $(EXTRA_LIB_PATHS),-cclib -L -cclib $(dir))
FRAMEWORK_OPTS=$(foreach framework, $(FRAMEWORKS),-cclib -framework -cclib $(framework))

BYTECODE_LINKER_FLAGS=$(NATIVE_OBJECT_FILES) $(NATIVE_LIB_OPTS) $(EXTRA_LIB_OPTS) $(FRAMEWORK_OPTS)
LINKER_FLAGS=$(BYTECODE_LINKER_FLAGS) $(SECTCREATE)


all: $(FLOWLIB) build-flow copy-flow-files
all-ocp: build-flow-with-ocp copy-flow-files-ocp

clean:
	ocamlbuild -clean
	rm -rf bin
	rm -f hack/utils/get_build_id.gen.c
	rm -f flow.odocl

build-flow: $(BUILT_OBJECT_FILES) $(FLOWLIB)
	ocamlbuild  -no-links  $(INCLUDE_OPTS) $(LIB_OPTS) -lflags "$(LINKER_FLAGS)" src/flow.native

build-flow-with-ocp: build-flow-stubs-with-ocp
	[ -d _obuild ] || ocp-build init
	ocp-build build flow

build-flow-debug: $(BUILT_OBJECT_FILES) $(FLOWLIB)
	ocamlbuild -lflags -custom -no-links $(INCLUDE_OPTS) $(LIB_OPTS) -lflags "$(LINKER_FLAGS)" src/flow.d.byte
	mkdir -p bin
	cp _build/src/flow.d.byte bin/flow

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

hack/utils/get_build_id.gen.c: FORCE
	OUT="const char* const BuildInfo_kRevision = \"$$(git rev-parse HEAD || hg id -i)\"; \
	const unsigned long BuildInfo_kRevisionCommitTimeUnix = $$(git log -1 --pretty=format:%ct || echo $$(hg log -r . -T \{date\} | grep -o ^[^.]* || echo 0))ul;"; \
	if [ "$$OUT" != "$$(cat hack/utils/get_build_id.gen.c 2>/dev/null)" ]; then echo "$$OUT" > hack/utils/get_build_id.gen.c; fi

build-flow-stubs-with-ocp:
	ocaml unix.cma scripts/gen_build_id.ml hack/utils/get_build_id.gen.c

# We only rebuild the flowlib archive if any of the libs have changed. If the
# archive has changed, then the incremental build needs to re-embed it into the
# binary. Unfortunately we rely on ocamlbuild to embed the archive on OSX and
# ocamlbuild isn't smart enough to understand dependencies outside of its
# automatic-dependency stuff.
$(FLOWLIB): $(wildcard lib/*)
	mkdir -p bin
	tar czf $@ -C lib .
ifeq ($(OS), Darwin)
	rm -f _build/src/flow.d.byte _build/src/flow.native
endif

copy-flow-files: build-flow $(FILES_TO_COPY)
	mkdir -p bin
ifeq ($(OS), Linux)
	objcopy --add-section flowlib=$(FLOWLIB) _build/src/flow.native bin/flow
else
	cp _build/src/flow.native bin/flow
endif

copy-flow-files-ocp: build-flow-with-ocp $(FLOWLIB) $(FILES_TO_COPY)
	mkdir -p bin
ifeq ($(OS), Linux)
	objcopy --add-section flowlib=$(FLOWLIB) _obuild/flow/flow.asm bin/flow
else
	cp _obuild/flow/flow.asm bin/flow
endif

do-test:
	./runtests.sh bin/flow

test: build-flow copy-flow-files
	${MAKE} do-test

test-ocp: build-flow-with-ocp copy-flow-files-ocp
	${MAKE} do-test

js: $(NATIVE_OBJECT_FILES)
	mkdir -p bin
	ocamlbuild -use-ocamlfind \
		-pkgs js_of_ocaml \
		-build-dir _build \
		-lflags -custom -no-links \
		$(INCLUDE_OPTS) $(LIB_OPTS) -lflags "$(BYTECODE_LINKER_FLAGS)" \
		src/flow_dot_js.byte
	# js_of_ocaml has no ability to upgrade warnings to errors, but we want to
	# error if, for example, there are any unimplemented C primitives.
	js_of_ocaml \
			--opt 3 \
			--disable genprim \
			-o bin/flow.js \
			$(JS_STUBS) _build/src/flow_dot_js.byte \
			2>_build/js_of_ocaml.err; \
	ret=$?; \
	if [ ! $$ret ]; then \
		exit $$ret; \
	elif [ -s _build/js_of_ocaml.err ]; then \
		printf "js_of_ocaml produced output on stderr:\n" 1>&2; \
		cat _build/js_of_ocaml.err 1>&2; \
		exit 1; \
	fi

FORCE:

.PHONY: all js FORCE

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
		| grep -v "src/parser/spider_monkey_ast.ml" \
		| grep -v "src/dts/dts_ast.ml" \
		| sed "s/\.ml$$//" > $@
	rm -f deps last_deps temp_deps

flow.docdir/index.html: flow.odocl
	ocamlbuild $(INCLUDE_OPTS) -use-ocamlfind flow.docdir/index.html

doc: flow.docdir/index.html
