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
  src/stubs\
  src/commands\
  src/common\
  src/embedded\
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
  hack/third-party/avl\
  hack/third-party/core\
  hack/$(INOTIFY)\
  hack/$(FSNOTIFY)

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
NATIVE_LIB_OPTS=$(foreach lib, $(NATIVE_LIBRARIES),-cclib -l -cclib $(lib))
NATIVE_LIB_DEBUG_OPTS=$(foreach lib, $(NATIVE_LIBRARIES),-dllib lib$(lib))
EXTRA_INCLUDE_OPTS=$(foreach dir, $(EXTRA_INCLUDE_PATHS),-ccopt -I -ccopt $(dir))
EXTRA_LIB_OPTS=$(foreach dir, $(EXTRA_LIB_PATHS),-cclib -L -cclib $(dir))
FRAMEWORK_OPTS=$(foreach framework, $(FRAMEWORKS),-cclib -framework -cclib $(framework))

LINKER_FLAGS=$(NATIVE_LIB_OPTS) $(EXTRA_LIB_OPTS) $(FRAMEWORK_OPTS) $(SECTCREATE)
LINKER_DEBUG_FLAGS=$(NATIVE_LIB_DEBUG_OPTS) $(EXTRA_LIB_OPTS) $(FRAMEWORK_OPTS) $(SECTCREATE)

PKGS=-pkgs deriving,deriving.syntax_tc,str,unix

PP=\
  `ocamlfind query -i-format -separator ' ' deriving type_conv`\
  pa_type_conv.cma\
  pa_deriving_common.cma\
  pa_deriving_classes.cma\
  pa_deriving_tc.cma

all: build-flowlib-archive build-flow copy-flow-files

clean:
	ocamlbuild -clean
	rm -rf bin
	rm -f hack/utils/get_build_id.gen.c

build-flow: _build/libcflow.a build-flowlib-archive
	ocamlbuild -pp "camlp4o $(PP)" $(PKGS) -no-links $(INCLUDE_OPTS) -lflags "libcflow.a $(LINKER_FLAGS)" src/flow.native

# CAML_LD_LIBRARY_PATH=CAML_LD_LIBRARY_PATH:./bin/ ocamldebug ./bin/flow-debug single ../test/
# Emacs:
# * M-x setenv CAML_LD_LIBRARY_PATH $CAML_LD_LIBRARY_PATH:/path/to/flow/bin/
# * M-X ocamldebug ./bin/flow-debug ocamldebug
# set arguments single ../../test
# goto 0
# load_printer debug_pp.cma
# install_printer Utils.SSet.Show_t.format
# install_printer Constraint_js.Show_context.format
# install_printer Constraint_js.Show_block.format
# install_printer Constraint_js.Show_bounds.format
# install_printer Custom_printer.Show_string_type_map.format
# install_printer Custom_printer.Show_int_string_type_map.format
debug: _build/debug.otarget _build/libcflow.a
	mkdir -p bin
	cp _build/dllcflow.so bin/
	cp _build/src/flow.d.byte bin/flow-debug

_build/debug.otarget: _build/libcflow.a build-flow-stubs build-flowlib-archive
	ocamlbuild -pp "camlp4o $(PP)" $(PKGS) -no-links $(INCLUDE_OPTS) -lflags "-dllib dllcflow.so $(LINKER_DEBUG_FLAGS)" debug.otarget

_build/libcflow.a: build-flow-stubs build-flowlib-archive
	ocamlbuild -pp "camlp4o $(PP)" $(PKGS) -no-links $(INCLUDE_OPTS) $(EXTRA_INCLUDE_OPTS) -lflags "$(LINKER_FLAGS)" -cflags "$(CC_OPTS)" libcflow.a

build-flow-stubs:
	echo 'const char* const BuildInfo_kRevision = "${SHA}";' > hack/utils/get_build_id.gen.c

build-flowlib-archive:
	mkdir -p bin
	tar czf bin/flowlib.tar.gz lib

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
