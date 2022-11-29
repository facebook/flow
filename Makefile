# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

################################################################################
#                                 Definitions                                  #
################################################################################

-include facebook/Makefile.defs

ifeq ($(OS), Windows_NT)
  EXE=.exe
  SWITCH=ocaml-variants.4.14.0+mingw64c
else
  EXE=
  SWITCH=ocaml-base-compiler.4.14.0
endif

JS_OF_OCAML_VERSION=4.0.0

# set FLOW_RELEASE=[1|true] or CI=true for an optimized build; otherwise,
# defaults to dev mode that builds faster but is less efficient at runtime.
ifndef FLOW_RELEASE
FLOW_RELEASE?=$(CI)
endif
ifeq ($(FLOW_RELEASE),false)
FLOW_RELEASE=
endif
ifeq ($(FLOW_RELEASE),0)
FLOW_RELEASE=
endif

DUNE_PROFILE=$(if $(FLOW_RELEASE),opt,dev)

NPM?=npm
YARN?=yarn

################################################################################
#                                    Rules                                     #
################################################################################

all: bin/flow$(EXE)

all-homebrew:
	export OPAMROOT="$(shell mktemp -d 2> /dev/null || mktemp -d -t opam)"; \
	export OPAMYES="1"; \
	export FLOW_RELEASE="1"; \
	opam init --bare --no-setup --disable-sandboxing && \
	rm -rf _opam && \
	opam switch create . --deps-only $(SWITCH) && \
	opam exec -- make

.PHONY: deps
deps:
	[ -d _opam ] || opam switch create . $(SWITCH) --deps-only --yes

.PHONY: deps-js
deps-js:
	opam install js_of_ocaml.$(JS_OF_OCAML_VERSION)

clean:
	if command -v dune >/dev/null; then dune clean; fi
	rm -rf _build
	rm -rf bin

.PHONY: bin/flow$(EXE)
bin/flow$(EXE):
	dune build --profile $(DUNE_PROFILE) src/flow.exe
	mkdir -p $(@D)
	install -C _build/default/src/flow.exe "$@"

.PHONY: ounit-tests
ounit-tests:
	dune runtest

.PHONY: ounit-tests-ci
ounit-tests-ci:
	mkdir -p test-results/ounit
	OUNIT_CI=true OUNIT_OUTPUT_JUNIT_FILE='$(shell pwd)/test-results/ounit/$$(suite_name).xml' dune runtest --force

do-test-js: bin/flow.js
	node src/__tests__/flow_dot_js_smoke_test.js $(realpath bin/flow.js)

do-test:
	./runtests.sh bin/flow$(EXE)
	bin/flow$(EXE) check packages/flow-dev-tools
	${MAKE} do-test-tool
	./tool test

do-test-tool:
	FLOW_BIN=../../bin/flow$(EXE) ${YARN} --cwd packages/flow-dev-tools test

test-tool: bin/flow$(EXE)
	${MAKE} do-test-tool

test-js: bin/flow.js do-test-js

test: bin/flow$(EXE) bin/flow.js
	${MAKE} do-test do-test-js

.PHONY: bin/flow.js
bin/flow.js:
	@mkdir -p $(@D)
	dune build --profile $(DUNE_PROFILE) src/flow_dot_js.bc.js
	install -C _build/default/src/flow_dot_js.bc.js "$@"

# Run `make js FLOW_RELEASE=1` to do an optimized build
js: bin/flow.js

dist/flow/flow$(EXE): bin/flow$(EXE)
	mkdir -p $(@D)
	install -C bin/flow$(EXE) "$@"

dist/flow.zip: dist/flow/flow$(EXE)
	cd dist && zip -r $(@F) flow/flow$(EXE)

dist/npm-%.tgz: FORCE
	@mkdir -p $(@D)
	@mkdir -p npm-$(*F)-tmp
	cd npm-$(*F)-tmp && ${NPM} pack ../packages/$(*F)/
	mv npm-$(*F)-tmp/$(*F)-*.tgz dist/npm-$(*F).tgz
	@rm -rf npm-$(*F)-tmp

FORCE:

.PHONY: all js FORCE

# Don't run in parallel because of https://github.com/ocaml/ocamlbuild/issues/300
.NOTPARALLEL:

-include facebook/Makefile

print-switch:
	@echo $(SWITCH)

print-jsoo-version:
	@echo $(JS_OF_OCAML_VERSION)
