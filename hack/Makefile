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
  find\
  format\
  globals\
  h2tp/common\
  h2tp/mapper\
  h2tp/test\
  h2tp/unparser\
  h2tp\
  heap\
  hh_matcher\
  hh_matcher/test\
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
  watchman\
  $(FSNOTIFY)\
  $(INOTIFY)

NATIVE_OBJECT_FILES=\
  heap/hh_shared.o\
  hhi/hhi_elf.o\
  hhi/hhi_win32res_stubs.o\
  utils/files.o\
  utils/get_build_id.gen.o\
  utils/get_build_id.o\
  utils/handle_stubs.o\
  utils/nproc.o\
  utils/realpath.o\
  utils/sysinfo.o\
  $(INOTIFY)/$(notdir $(INOTIFY))_stubs.o

OCAML_LIBRARIES=\
  str\
  unix\
  bigarray

NATIVE_LIBRARIES=\
  pthread\
  $(ELF)

TARGETS=_build/hh_server.native _build/hh_client.native \
		_build/hh_single_type_check.native _build/hh_emitter.native \
		_build/hh_format.native _build/h2tp.native _build/test_unparser.native \
		_build/hh_match.native \
		_build/code_extent_tests.native \
		_build/matcher_test.native \
		_build/patcher_api_test.native \
		_build/patcher_module_test.native \
		_build/patcher_test.native

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
all-ocp: build-hhi-archive build-hack-with-ocp copy-hack-files-ocp

clean:
	ocamlbuild -clean
	find ../bin -mindepth 1 -not -path ../bin/README -delete
	rm -f utils/get_build_id.gen.c

build-hack: $(TARGETS) build-hhi-archive

build-hack-with-ocp: build-hhi-archive build-hack-stubs-with-ocp
	[ -d ${ROOT}/../_obuild ] || ( cd ${ROOT}/.. && ocp-build init )
	ocp-build build

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

# If you modify this rule, do not forget to edit the file
# `$(ROOT)/scripts/gen_build_id.ml` used on Windows.
build-hack-stubs:
	REV="const char* const BuildInfo_kRevision = \"$$(git rev-parse HEAD || hg id -i)\";"; \
	if [ "$$REV" != "$$(cat utils/get_build_id.gen.c 2>/dev/null)" ]; then echo "$$REV" > utils/get_build_id.gen.c; fi

build-hack-stubs-with-ocp:
	cd $(ROOT)/.. && \
        ocaml unix.cma scripts/gen_build_id.ml src/utils/get_build_id.gen.c

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
	cp _build/hh_emitter.native ../bin/hh_emitter
	cp _build/hh_format.native ../bin/hh_format
	cp _build/h2tp/h2tp.native ../bin/h2tp
	cp _build/hh_match.native ../bin/hh_match

copy-hack-files-ocp: build-hack-with-ocp
	mkdir -p ../bin
ifeq ($(OS), Linux)
	objcopy --add-section hhi=../bin/hhi.tar.gz ../_obuild/hh_server/hh_server.asm ../bin/hh_server
else
	cp ../_obuild/hh_server/hh_server.asm ../bin/hh_server
endif
	cp ../_obuild/hh_client/hh_client.asm ../bin/hh_client
	cp ../_obuild/hh_single_type_check/hh_single_type_check.asm ../bin/hh_single_type_check
	cp ../_obuild/hh_format/hh_format.asm ../bin/hh_format
	cp ../_obuild/h2tp/h2tp.asm ../bin/h2tp
	cp ../_obuild/hh_match/hh_match.asm ../bin/hh_match

copy-match-test-files:
	mkdir -p ../bin
	cp _build/hh_matcher/test/code_extent_tests.native \
		../bin/code_extent_test_driver
	cp _build/hh_matcher/test/matcher_test.native \
		../bin/hh_match_test_driver
	cp _build/hh_matcher/test/patcher_module_test.native \
		../bin/hh_patch_module_test_driver
	cp _build/hh_matcher/test/patcher_api_test.native \
		../bin/hh_patch_api_test_driver
	cp _build/hh_matcher/test/patcher_test.native \
		../bin/hh_patch_test_driver

copy-match-test-files-ocp:
	mkdir -p ../bin
	cp ../_obuild/code_extent_tests/code_extent_tests.asm \
		../bin/code_extent_test_driver
	cp ../_obuild/matcher_test/matcher_test.asm \
		../bin/hh_match_test_driver
	cp ../_obuild/patcher_module_test/patcher_module_test.asm \
		../bin/hh_patch_module_test_driver
	cp ../_obuild/patcher_api_test/patcher_api_test.asm \
		../bin/hh_patch_api_test_driver
	cp ../_obuild/patcher_test/patcher_test.asm \
		../bin/hh_patch_test_driver

.PHONY: test test-ocp do-test
test: build-hack copy-hack-files copy-match-test-files
	${MAKE} do-test

test-ocp: build-hack-with-ocp copy-hack-files-ocp copy-match-test-files-ocp
	${MAKE} do-test

do-test:
	python3 ../test/verify.py --program ../bin/hh_single_type_check ../test/autocomplete
	python3 ../test/verify.py --program ../bin/hh_single_type_check ../test/color
	python3 ../test/verify.py --program ../bin/hh_single_type_check ../test/colour
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
	python3 ../test/verify.py --program ../bin/code_extent_test_driver ../test/hh_matcher/code_extent
	python3 ../test/verify.py --program ../bin/hh_match_test_driver ../test/hh_matcher/matcher_tests
	python3 ../test/verify.py --program ../bin/hh_patch_module_test_driver ../test/hh_matcher/patcher_module_tests
	python3 ../test/verify.py --program ../bin/hh_patch_api_test_driver ../test/hh_matcher/patcher_api_tests
	python3 ../test/verify.py --program ../bin/hh_patch_test_driver ../test/hh_matcher/patcher_tests
	python3 ../test/integration/runner.py ../bin/hh_server ../bin/hh_client
