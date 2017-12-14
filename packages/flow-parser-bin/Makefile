# Copyright (c) 2017-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

OCAML_HEADERS=\
	include/caml/alloc.h \
	include/caml/callback.h \
	include/caml/config.h \
	include/caml/memory.h \
	include/caml/misc.h \
	include/caml/mlvalues.h

OCAML_PATH=$(shell ocamlc -where)

.PHONY: lib/libflowparser.a
lib/libflowparser.a:
	@mkdir -p lib
	$(MAKE) -C ../../src/parser lib/libflowparser.a
	cp ../../src/parser/lib/libflowparser.a "$@"

include/flowparser/libflowparser.h: ../../src/parser/libflowparser.h
	@mkdir -p "$(dir $@)"
	cp "$<" "$@"

$(OCAML_HEADERS): include/%: $(OCAML_PATH)/%
	@mkdir -p "$(dir $@)"
	cp "$<" "$@"

.PHONY: deps
deps: lib/libflowparser.a include/flowparser/libflowparser.h $(OCAML_HEADERS)

.PHONY: clean
clean:
	rm -rf include/ lib/
