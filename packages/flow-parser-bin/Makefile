# Copyright (c) 2017-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

OCAML_PATH=$(shell ocamlc -where)
OCAML_HEADERS=\
	$(sort $(patsubst $(OCAML_PATH)/%,include/%, \
		$(filter $(OCAML_PATH)/%,$(shell \
			$(CC) -I $(OCAML_PATH) -MM -MT deps ../../src/parser/libflowparser.h \
	))))

all: deps

lib/mac/libflowparser.a lib/linux/libflowparser.a lib/win/libflowparser.a:
	@mkdir -p "$(dir $@)"
	$(MAKE) -C ../../src/parser lib/libflowparser.a
	cp ../../src/parser/lib/libflowparser.a "$@"

include/flowparser/libflowparser.h: ../../src/parser/libflowparser.h
	@mkdir -p "$(dir $@)"
	cp "$<" "$@"

$(OCAML_HEADERS): include/%: $(OCAML_PATH)/%
	@mkdir -p "$(dir $@)"
	cp "$<" "$@"

.PHONY: headers
headers: include/flowparser/libflowparser.h $(OCAML_HEADERS)

deps-linux: headers lib/linux/libflowparser.a
deps-mac: headers lib/mac/libflowparser.a
deps-win: headers lib/win/libflowparser.a

clean:
	rm -rf include/ lib/

.PHONY: all deps clean
