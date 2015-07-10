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
  client\
  deps\
  dfind\
  emitter\
  format\
  globals\
  h2tp/common\
  h2tp/mapper\
  h2tp/test\
  h2tp/unparser\
  h2tp\
  heap\
  hhi\
  naming\
  parsing\
  procs\
  search\
  server\
  socket\
  stubs\
  third-party/avl\
  third-party/core\
  typing\
  utils\
  $(FSNOTIFY)\
  $(INOTIFY)

NATIVE_OBJECT_FILES=\
  heap/hh_shared.o\
  hhi/hhi_elf.o\
  utils/get_build_id.gen.o\
  utils/get_build_id.o\
  utils/nproc.o\
  utils/realpath.o\
  $(INOTIFY)/$(notdir $(INOTIFY))_stubs.o

OCAML_LIBRARIES=\
  str\
  unix

NATIVE_LIBRARIES=\
  pthread\
  $(ELF)

TARGETS=_build/hh_server.native _build/hh_client.native _build/hh_single_type_check.native \
		_build/hh_format.native _build/h2tp.native _build/test_unparser.native

# Find all source files (all files not in _build dir)
ALL_SRC_FILES=$(call rwildcard,$(patsubst _build,,$(wildcard *)),*.*)

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
# Function to recursively find files, eg: $(call rwildcard,dir/to/look/in/,*.c)
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

all: build-hhi-archive build-hack copy-hack-files

clean:
	ocamlbuild -clean
	find ../bin -mindepth 1 -not -path ../bin/README -delete
	rm -f utils/get_build_id.gen.c

build-hack: $(TARGETS) build-hhi-archive

# All targets are built in one time, so no parallelization is necessary
.NOTPARALLEL: $(TARGETS)
# As there is no efficient way to calculate the dependencies of
# the targets, we make them dependent on all files. In doing this
# we ensure that no rebuild is necessary if nothing has changed
$(TARGETS): $(ALL_SRC_FILES)
	# build-hack-native-deps is a dependency of $(TARGETS) but as it is phony
	# we place it here to avoid unnecessary rebuilds
	$(MAKE) build-hack-native-deps build-hhi-archive
	ocamlbuild -no-links -cflag -g $(INCLUDE_OPTS) $(LIB_OPTS) \
		-lflags "-g $(LINKER_FLAGS)" \
		$(patsubst _build/%,%,$(TARGETS))
	# Touching the targets is necessary because the ocaml build
	# doesn't change the modification dates of the targets if
	# the new binaries are exactly the same as the old ones
	touch $(TARGETS)

build-hack-native-deps: build-hack-stubs
	ocamlbuild -cflags "-g $(EXTRA_INCLUDE_OPTS) $(EXTRA_CC_OPTS)" $(NATIVE_OBJECT_FILES)

build-hack-stubs:
	REV="const char* const BuildInfo_kRevision = \"$$(git rev-parse HEAD)\";"; \
	if [ "$$REV" != "$$(cat utils/get_build_id.gen.c)" ]; then echo "$$REV" > utils/get_build_id.gen.c; fi

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
	python3 ../test/verify.py --program ../bin/hh_single_type_check ../test/autocomplete
	python3 ../test/verify.py --program ../bin/hh_single_type_check ../test/color
	python3 ../test/verify.py --program ../bin/hh_single_type_check ../test/coverage
	python3 ../test/verify.py --program ../bin/hh_single_type_check ../test/dumpsymbolinfo
	python3 ../test/verify.py --program ../bin/hh_format ../test/format
	python3 ../test/verify.py --program ../bin/hh_single_type_check ../test/suggest
	python3 ../test/verify.py --program ../bin/hh_single_type_check ../test/typecheck
	python3 ../test/verify.py --program ../bin/hh_format ../test/typecheck \
		--disabled-extension .no_format \
		--out-extension .format_out \
		--expect-extension '' \
		--flags --root .
