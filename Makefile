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
  INOTIFY=inotify
  FSNOTIFY=fsnotify_linux
  ELF=elf
  FRAMEWORKS=
endif
ifeq ($(OS), Darwin)
  INOTIFY=fsevents
  FSNOTIFY=fsnotify_darwin
  ELF=
  FRAMEWORKS=CoreServices CoreFoundation
endif

################################################################################
#                                 Definitions                                  #
################################################################################

MODULES=\
  src/stubs\
  src/commands\
  src/common\
  src/dts\
  src/typing\
  src/parser\
  src/server\
  src/parsing\
  hack/utils\
  hack/client\
  hack/socket\
  hack/server\
  hack/stubs\
  hack/typing\
  hack/naming\
  hack/parsing\
  hack/deps\
  hack/heap\
  hack/globals\
  hack/procs\
  hack/search\
  hack/hhi\
  hack/dfind\
  hack/avl\
  hack/$(INOTIFY)\
  hack/$(FSNOTIFY)

NATIVE_OBJECT_FILES=\
  hack/heap/hh_shared.o\
  hack/utils/realpath.o\
  hack/$(INOTIFY)/$(INOTIFY)_stubs.o\
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

INCLUDE_OPTS=$(foreach dir,$(MODULES),-I $(dir))
LIB_OPTS=$(foreach lib,$(OCAML_LIBRARIES),-lib $(lib))
NATIVE_LIB_OPTS=$(foreach lib, $(NATIVE_LIBRARIES),-cclib -l -cclib $(lib))
EXTRA_INCLUDE_OPTS=$(foreach dir, $(EXTRA_INCLUDE_PATHS),-ccopt -I -ccopt $(dir))
EXTRA_LIB_OPTS=$(foreach dir, $(EXTRA_LIB_PATHS),-cclib -L -cclib $(dir))
FRAMEWORK_OPTS=$(foreach framework, $(FRAMEWORKS),-cclib -framework -cclib $(framework))

LINKER_FLAGS=$(NATIVE_OBJECT_FILES) $(NATIVE_LIB_OPTS) $(EXTRA_LIB_OPTS) $(FRAMEWORK_OPTS)

all: build-flow copy-flow-files

clean:
	ocamlbuild -clean
	rm -rf bin
	rm -f hack/utils/get_build_id.gen.c

build-flow: build-flow-native-deps
	ocamlbuild -no-links  $(INCLUDE_OPTS) $(LIB_OPTS) -lflags "$(LINKER_FLAGS)" src/flow.native

build-flow-native-deps: build-flow-stubs
	ocamlbuild -cflags "$(EXTRA_INCLUDE_OPTS)" $(NATIVE_OBJECT_FILES)

build-flow-stubs:
	echo 'const char* const BuildInfo_kRevision = "${SHA}";' > hack/utils/get_build_id.gen.c

copy-flow-files: build-flow $(FILES_TO_COPY)
	mkdir -p bin
	cp _build/src/flow.native bin/flow
	mkdir -p bin/lib
	cp $(FILES_TO_COPY) bin/lib

test: build-flow copy-flow-files
	./runtests.sh bin/flow
