# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

FLOW_VERSION ?= $(shell node -p 'require("./package.json").version')
HOST ?= https://github.com/facebook/flow/releases/download/v$(FLOW_VERSION)
PLATFORMS = linux64 osx # TODO: win64
LIBS = $(patsubst %,lib/%/libflowparser.a,$(PLATFORMS))
RELEASE_ZIPS = $(patsubst %,dist/release/libflowparser-%.zip,$(PLATFORMS))

ifeq ($(OS), Windows_NT)
	UNAME_S=Windows
else
	UNAME_S=$(shell uname -s)
endif
ifeq ($(UNAME_S), Linux)
	PLATFORM=linux64
endif
ifeq ($(UNAME_S), Windows)
	PLATFORM=win64
endif
ifeq ($(UNAME_S), Darwin)
	PLATFORM=osx
endif

.SUFFIXES:

.PHONY: local
local: include lib/$(PLATFORM)/libflowparser.a

.PHONY: dist
dist: include SHASUM256.txt
	touch .premade

.PHONY: download
download: $(addsuffix .dl,$(RELEASE_ZIPS))

.PHONY: clean
clean:
	rm -rf .premade SHASUM256.txt include/ lib/ dist/ build/

# Phony targets for each platform's prebuilt zip file to trigger downloading
# them from GitHub. Below, we rely on the fact that the zips themselves don't
# have a build target, in order to prefer building from source when
# `make download` hasn't been run.
$(addsuffix .dl,$(RELEASE_ZIPS)):
	@mkdir -p "$(@D)"
	[ -e "$(subst .dl,,$@)" ] || curl --max-time 300 \
		-L "$(patsubst dist/release/%.zip.dl,$(HOST)/%-v$(FLOW_VERSION).zip,$@)" \
		-o "$(subst .dl,,$@)" || rm "$(subst .dl,,$@)"

# Builds libflowparser.zip for the current platform from source. Phony so that
# ocamlbuild can decide if anything has changed.
.PHONY: dist/local/libflowparser-$(PLATFORM).zip
dist/local/libflowparser-$(PLATFORM).zip:
	@mkdir -p "$(@D)"
	$(MAKE) -C ../../src/parser dist/libflowparser.zip
	[ -e "$@" -a "$@" -nt "../../src/parser/dist/libflowparser.zip" ] || \
		cp ../../src/parser/dist/libflowparser.zip "$@"

# The following two "pattern" rules (because of the %) choose between using
# the downloaded release builds or building from source, depending on whether
# the release builds exist. Make doesn't error when a pattern rule's prereq
# doesn't exist; it tries the next one instead. Here is where we rely on there
# being no build target for $(RELEASE_ZIPS). If the release zip exists, then
# the first rule matches; if it doesn't, then it chooses the 2nd rule which
# kicks off the local build.
dist/libflowparser-%.zip: dist/release/libflowparser-%.zip
	@mkdir -p "$(@D)"
	cp "$<" "$@"
dist/libflowparser-%.zip: dist/local/libflowparser-%.zip
	@mkdir -p "$(@D)"
	cp "$<" "$@"

# if `.premade` exists, don't try to build
lib/%/libflowparser.a: .premade ;
lib/%/libflowparser.a: dist/libflowparser-%.zip
	DIR="$(basename $<)"; \
	mkdir -p "$$DIR" "$(@D)"; \
	unzip "$<" libflowparser/lib/libflowparser.a -d "$$DIR" && \
	mv "$$DIR/libflowparser/lib/libflowparser.a" "$@"; \
	rm -rf "$$DIR"
	touch "$@"

# if `.premade` exists, don't try to build
include%: .premade ;
include%: dist/libflowparser-$(PLATFORM).zip
	DIR="$(basename $<)"; \
	mkdir -p "$$DIR"; \
	unzip "$<" "libflowparser/include/**" -d "$$DIR" && \
	mv "$$DIR/libflowparser/include" include; \
	rm -rf "$$DIR"

include: include.made

SHASUM256.txt: $(LIBS)
	shasum -a 256 $^ > $@
