# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Usage:
#   make compile           Compile eval dirs → build/swebench/flow_evals.jsonl
#   make list              List all eval names
#   make run               Compile + run all evals against Claude (pass ARGS="--model ..." etc.)
#   make dry-run           Compile + validate pipeline with gold patches (no API calls)
#   make validate          Alias for dry-run
#   make clean             Remove build/
#
# Flow binary: by default the `flow` from the flow-bin npm package is used, and
# `npm install` is run automatically the first time if node_modules/ is missing.
# Point at a locally built binary with FLOW_BIN (skips the install), e.g.
#   make run FLOW_BIN=/path/to/flow
#
# Filtering (pass via ARGS, repeatable): runs a subset of evals.
#   make dry-run ARGS="--category ts_to_flow"     Only the Category 4 (TS->Flow) evals
#   make run     ARGS="--category ts_to_flow --model claude-sonnet-5"
#   make dry-run ARGS="--tag match"               Only evals tagged 'match'
#   make dry-run ARGS="--eval variance_advanced"  By name (substring/glob)

EVALS_SRC := $(shell find evals -type f 2>/dev/null)

.PHONY: compile list run run-sequential dry-run validate clean

FLOW_BIN ?= node_modules/.bin/flow

# Only auto-install the bundled Flow binary when the default is in use; a
# caller-supplied FLOW_BIN (e.g. a locally built binary) skips `npm install`.
ifeq ($(FLOW_BIN),node_modules/.bin/flow)
FLOW_DEP := node_modules
endif

node_modules:
	npm install

compile: build/swebench/.stamp

build/swebench/.stamp: $(EVALS_SRC) compile_swebench.py
	python3 compile_swebench.py
	@touch $@

list: compile
	python3 run_swebench.py --list-inputs $(ARGS)

run: compile $(FLOW_DEP)
	python3 run_swebench.py --flow-bin $(FLOW_BIN) -j 4 $(ARGS)

run-sequential: compile $(FLOW_DEP)
	python3 run_swebench.py --flow-bin $(FLOW_BIN) $(ARGS)

dry-run: compile $(FLOW_DEP)
	python3 run_swebench.py --dry-run --flow-bin $(FLOW_BIN) $(ARGS)

validate: dry-run

clean:
	rm -rf build/
