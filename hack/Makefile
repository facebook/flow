# Copyright (c) 2014, Facebook, Inc.
# All rights reserved.

################################################################################
#                            Variables to override                             #
################################################################################

EXTRA_INCLUDE_PATHS=
EXTRA_LIB_PATHS=
EXTRA_CC_FLAGS=

################################################################################
#                              OS-dependent stuff                              #
################################################################################

OS=$(shell uname -s)
ROOT=$(shell pwd)

ifeq ($(OS), Linux)
  INOTIFY=third-party/inotify
  FSNOTIFY=fsnotify_linux
  ELF=elf
  FRAMEWORKS=
  SECTCREATE=
endif
ifeq ($(OS), Darwin)
  INOTIFY=fsevents
  FSNOTIFY=fsnotify_darwin
  ELF=
  FRAMEWORKS=CoreServices CoreFoundation
  SECTCREATE=-cclib -sectcreate -cclib __text -cclib hhi -cclib $(ROOT)/../bin/hhi.tar.gz
endif

################################################################################
#                                 Definitions                                  #
################################################################################

MODULES=\
  utils\
  client\
  socket\
  server\
  stubs\
  typing\
  naming\
  parsing\
  format\
  deps\
  heap\
  globals\
  procs\
  search\
  hhi\
  dfind\
  h2tp/common\
  h2tp/mapper\
  h2tp/unparser\
  h2tp/test\
  h2tp\
  third-party/avl\
  $(INOTIFY)\
  $(FSNOTIFY)

NATIVE_OBJECT_FILES=\
  heap/hh_shared.o\
  utils/realpath.o\
  $(INOTIFY)/$(notdir $(INOTIFY))_stubs.o\
  utils/nproc.o\
  hhi/hhi_elf.o\
  utils/get_build_id.gen.o\
  utils/get_build_id.o

OCAML_LIBRARIES=\
  unix\
  str

NATIVE_LIBRARIES=\
  $(ELF)\
  pthread

################################################################################
#                                    Rules                                     #
################################################################################

INCLUDE_OPTS=$(foreach dir,$(MODULES),-I $(dir))
LIB_OPTS=$(foreach lib,$(OCAML_LIBRARIES),-lib $(lib))
NATIVE_LIB_OPTS=$(foreach lib, $(NATIVE_LIBRARIES),-cclib -l$(lib))
EXTRA_NATIVE_LIB_OPTS=$(foreach lib, $(EXTRA_NATIVE_LIBRARIES),-cclib -l$(lib))
EXTRA_INCLUDE_OPTS=$(foreach dir, $(EXTRA_INCLUDE_PATHS),-ccopt -I$(dir))
EXTRA_CC_OPTS=$(foreach opt, $(EXTRA_CC_FLAGS),-ccopt $(opt))
EXTRA_LIB_OPTS=$(foreach dir, $(EXTRA_LIB_PATHS),-cclib -L$(dir))
FRAMEWORK_OPTS=$(foreach framework, $(FRAMEWORKS),-cclib -framework -cclib $(framework))

LINKER_FLAGS=$(NATIVE_OBJECT_FILES) $(NATIVE_LIB_OPTS) $(EXTRA_LIB_OPTS) \
	     $(EXTRA_NATIVE_LIB_OPTS) $(FRAMEWORK_OPTS) $(SECTCREATE)

all: build-hhi-archive build-hack copy-hack-files

clean:
	ocamlbuild -clean
	find ../bin -mindepth 1 -not -path ../bin/README -delete
	rm -f utils/get_build_id.gen.c

build-hack: build-hack-native-deps build-hhi-archive
	ocamlbuild -no-links $(INCLUDE_OPTS) $(LIB_OPTS) \
		-lflags "$(LINKER_FLAGS)" \
		hh_server.native hh_client.native hh_single_type_check.native \
		hh_format.native h2tp.native test_unparser.native

build-hack-native-deps: build-hack-stubs
	# ocamlbuild before 4.00 does not pass thru cflags when building native C
	# objects, because they're not important, right? So use this super ugly hack.
	ocamlbuild -ocamlc "ocamlopt $(EXTRA_INCLUDE_OPTS) $(EXTRA_CC_OPTS)" $(NATIVE_OBJECT_FILES)

build-hack-stubs:
	echo "const char* const BuildInfo_kRevision = \"$$(git rev-parse HEAD)\";" > utils/get_build_id.gen.c

build-hhi-archive:
	mkdir -p ../bin
	$(MAKE) -C ../hhi

copy-hack-files: build-hack
	mkdir -p ../bin
ifeq ($(OS), Linux)
	objcopy --add-section hhi=../bin/hhi.tar.gz _build/hh_server.native ../bin/hh_server
else
	cp _build/hh_server.native ../bin/hh_server
endif
	cp _build/hh_client.native ../bin/hh_client
	cp _build/hh_single_type_check.native ../bin/hh_single_type_check
	cp _build/hh_format.native ../bin/hh_format
	cp _build/h2tp/h2tp.native ../bin/h2tp

test: build-hack copy-hack-files
	python3 ../test/verify.py --program ../bin/hh_single_type_check ../test/color
	python3 ../test/verify.py --program ../bin/hh_single_type_check ../test/coverage
	python3 ../test/verify.py --program ../bin/hh_format ../test/format
	python3 ../test/verify.py --program ../bin/hh_single_type_check ../test/suggest
	python3 ../test/verify.py --program ../bin/hh_single_type_check ../test/typecheck
	python3 ../test/verify.py --program ../bin/hh_format ../test/typecheck \
		--disabled-extension .no_format \
		--out-extension .format_out \
		--expect-extension '' \
		--flags --root .
