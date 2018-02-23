# Copyright (c) 2017-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

FLOW_VERSION ?= $(shell node -p 'require("./package.json").version')
HOST ?= https://github.com/facebook/flow/releases/download/v$(FLOW_VERSION)
PLATFORMS = linux64 osx # TODO: win64
LIBS = $(patsubst %,lib/%/libflowparser.a,$(PLATFORMS))
ZIPS = $(patsubst %,dist/libflowparser-%.zip,$(PLATFORMS))

all: include SHASUM256.txt

$(ZIPS):
	@mkdir -p "$(@D)"
	curl --max-time 300 -L "$(patsubst dist/%.zip,$(HOST)/%-v$(FLOW_VERSION).zip,$@)" -o "$@" || rm "$@"

$(LIBS): lib/%/libflowparser.a: dist/libflowparser-%.zip
	DIR="$(basename $<)"; \
	mkdir -p "$$DIR" "$(@D)"; \
	unzip "$<" libflowparser/lib/libflowparser.a -d "$$DIR" && \
	mv "$$DIR/libflowparser/lib/libflowparser.a" "$@"; \
	rm -rf "$$DIR"
	touch "$@"

include: dist/libflowparser-linux64.zip
	DIR="$(basename $<)"; \
	mkdir -p "$$DIR" "$(@D)"; \
	unzip "$<" "libflowparser/include/**" -d "$$DIR" && \
	mv "$$DIR/libflowparser/include" "$@"; \
	rm -rf "$$DIR"

SHASUM256.txt: $(LIBS)
	shasum -a 256 $^ > $@

clean:
	rm -rf SHASUM256.txt include/ lib/ dist/ build/

.PHONY: all deps clean
