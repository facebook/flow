# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

################################################################################
#                                 Definitions                                  #
################################################################################

ifeq ($(OS), Windows_NT)
  EXE=.exe
else
  EXE=
endif

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

FLOW_JS_IMPL?=rust-wasm

NPM?=npm
NODE?=node
YARN?=yarn

################################################################################
#                                    Rules                                     #
################################################################################

do-test-js: bin/flow.js
	$(NODE) src/__tests__/flow_dot_js_smoke_test.js $(realpath bin/flow.js)

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
ifeq ($(FLOW_JS_IMPL),rust-wasm)
bin/flow.js:
	FLOW_RELEASE="$(FLOW_RELEASE)" scripts/build-flow-dot-js-wasm.sh --output "$@"
else
$(error Unknown FLOW_JS_IMPL '$(FLOW_JS_IMPL)'. Expected rust-wasm)
endif

# Run `make js FLOW_RELEASE=1` to do an optimized build
js: bin/flow.js

js-rust:
	${MAKE} js FLOW_JS_IMPL=rust-wasm

test-js-rust:
	${MAKE} test-js FLOW_JS_IMPL=rust-wasm

dist/npm-%.tgz: FORCE
	@mkdir -p $(@D)
	@mkdir -p npm-$(*F)-tmp
	cd npm-$(*F)-tmp && ${NPM} pack ../packages/$(*F)/
	mv npm-$(*F)-tmp/$(*F)-*.tgz dist/npm-$(*F).tgz
	@rm -rf npm-$(*F)-tmp

FORCE:

.PHONY: js js-rust test-js-rust FORCE
