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
  SECTCREATE=-cclib -sectcreate -cclib __text -cclib flowlib -cclib $(abspath bin/flowlib.tar.gz)
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
  src/stubs\
  src/typing\
  hack/deps\
  hack/dfind\
  hack/globals\
  hack/heap\
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
  src/embedded/flowlib_elf.o\
  hack/heap/hh_shared.o\
  hack/utils/realpath.o\
  hack/$(INOTIFY_STUBS)\
  hack/utils/nproc.o\
  hack/hhi/hhi_elf.o\
  hack/utils/get_build_id.gen.o\
  hack/utils/get_build_id.o

OCAML_LIBRARIES=\
  unix\
  str

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


all: build-flowlib-archive build-flow copy-flow-files

clean:
	ocamlbuild -clean
	rm -rf bin
	rm -f hack/utils/get_build_id.gen.c

build-flow: build-flow-native-deps build-flowlib-archive
	ocamlbuild  -no-links  $(INCLUDE_OPTS) $(LIB_OPTS) -lflags "$(LINKER_FLAGS)" src/flow.native

build-flow-debug: build-flow-native-deps build-flowlib-archive
	ocamlbuild -lflags -custom -no-links $(INCLUDE_OPTS) $(LIB_OPTS) -lflags "$(LINKER_FLAGS)" src/flow.d.byte
	mkdir -p bin
	cp _build/src/flow.d.byte bin/flow

build-flow-native-deps: build-flow-stubs
	ocamlbuild -ocamlc "ocamlopt $(EXTRA_INCLUDE_OPTS) $(CC_OPTS)"\
		$(NATIVE_OBJECT_FILES)

build-flow-stubs:
	echo "const char* const BuildInfo_kRevision = \"$$(git rev-parse HEAD)\";" > hack/utils/get_build_id.gen.c

build-flowlib-archive:
	mkdir -p bin
	cd lib && tar czf ../bin/flowlib.tar.gz ./* && cd ..

copy-flow-files: build-flow $(FILES_TO_COPY)
	mkdir -p bin
	cp -r examples bin/examples
ifeq ($(OS), Linux)
	objcopy --add-section flowlib=bin/flowlib.tar.gz _build/src/flow.native bin/flow
else
	cp _build/src/flow.native bin/flow
endif

test: build-flow copy-flow-files
	./runtests.sh bin/flow
