# Copyright (c) 2014, Facebook, Inc.
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
  src/services/inference\
  src/stubs\
  src/typing\
  hack/deps\
  hack/dfind\
  hack/find\
  hack/globals\
  hack/heap\
  hack/hhi\
  hack/parsing\
  hack/procs\
  hack/search\
  hack/socket\
  hack/stubs\
  hack/third-party/avl\
  hack/third-party/core\
  hack/utils\
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
  $(ELF)

FILES_TO_COPY=\
  $(wildcard lib/*.js)

################################################################################
#                                    Rules                                     #
################################################################################

CC_FLAGS=-DNO_LZ4
CC_OPTS=$(foreach flag, $(CC_FLAGS), -ccopt $(flag))
INCLUDE_OPTS=$(foreach dir,$(MODULES),-I $(dir))
LIB_OPTS=$(foreach lib,$(OCAML_LIBRARIES),-lib $(lib))
NATIVE_LIB_OPTS=$(foreach lib, $(NATIVE_LIBRARIES),-cclib -l -cclib $(lib))
EXTRA_INCLUDE_OPTS=$(foreach dir, $(EXTRA_INCLUDE_PATHS),-ccopt -I -ccopt $(dir))
EXTRA_LIB_OPTS=$(foreach dir, $(EXTRA_LIB_PATHS),-cclib -L -cclib $(dir))
FRAMEWORK_OPTS=$(foreach framework, $(FRAMEWORKS),-cclib -framework -cclib $(framework))

LINKER_FLAGS=$(NATIVE_OBJECT_FILES) $(NATIVE_LIB_OPTS) $(EXTRA_LIB_OPTS) $(FRAMEWORK_OPTS) $(SECTCREATE)


all: $(FLOWLIB) build-flow copy-flow-files
all-ocp: build-flow-with-ocp copy-flow-files-ocp

clean:
	ocamlbuild -clean
	rm -rf bin
	rm -f hack/utils/get_build_id.gen.c

build-flow: build-flow-native-deps $(FLOWLIB)
	ocamlbuild  -no-links  $(INCLUDE_OPTS) $(LIB_OPTS) -lflags "$(LINKER_FLAGS)" src/flow.native

build-flow-with-ocp: build-flow-stubs-with-ocp
	[ -d _obuild ] || ocp-build init
	ocp-build build flow

build-flow-debug: build-flow-native-deps $(FLOWLIB)
	ocamlbuild -lflags -custom -no-links $(INCLUDE_OPTS) $(LIB_OPTS) -lflags "$(LINKER_FLAGS)" src/flow.d.byte
	mkdir -p bin
	cp _build/src/flow.d.byte bin/flow

build-flow-native-deps: build-flow-stubs
	ocamlbuild -ocamlc "ocamlopt $(EXTRA_INCLUDE_OPTS) $(CC_OPTS)"\
		$(NATIVE_OBJECT_FILES)

build-flow-stubs:
	echo "const char* const BuildInfo_kRevision = \"$$(git rev-parse HEAD 2>/dev/null || hg identify -i)\";" > hack/utils/get_build_id.gen.c

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
