#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Grader: no_extra_flow_errors (trajectory grader)
# Penalizes runs where the agent hit too many erroneous Flow type-check results
# during the trajectory.
#
# For error_fixing evals (Cat 1) one Flow error is expected: the initial check
# that demonstrates the problem to the agent. Set FLOW_ERROR_THRESHOLD=1.
#
# For code_writing evals (Cat 2/3) no errors are expected. Set
# FLOW_ERROR_THRESHOLD=0 explicitly (the default is 1 to cover Cat 1).
#
# An "erroneous flow call" is a tool result with is_error=true whose content
# begins with "Exit code" (a non-zero exit) AND whose originating Bash command
# actually *executed* Flow (e.g. `./flow check .`, `$FLOW_BIN .`, bare `flow`).
# Detection is by command position (see is_flow_invocation): a command that only
# names a flow path as an argument to another program — `ls node_modules/.bin/flow`,
# `grep ... /repo/flow/...`, `readlink ./flow` — is NOT a Flow run and is ignored,
# matching the grader's name. Foreign tooling such as `tsc` is handled by the
# separate `no_tsc` grader.
#
# When no trajectory exists (dry-run mode), this grader always passes.
#
# Usage: no_extra_flow_errors.sh
# Env:   TRAJECTORY_PATH      - path to the trajectory JSONL (optional)
#        FLOW_ERROR_THRESHOLD  - max allowed erroneous flow calls (default: 1)
# Exit code: 0 = pass, 1 = fail

set -euo pipefail

TRAJECTORY_PATH="${TRAJECTORY_PATH:-}"
FLOW_ERROR_THRESHOLD="${FLOW_ERROR_THRESHOLD:-1}"

if [[ -z "$TRAJECTORY_PATH" || ! -f "$TRAJECTORY_PATH" ]]; then
  echo '{"pass": true, "reason": "no trajectory available (dry-run or missing)"}'
  exit 0
fi

COUNT=$(python3 - "$TRAJECTORY_PATH" <<'PYEOF'
import json
import re
import shlex
import sys

traj_path = sys.argv[1]
error_count = 0

# Decide whether a Bash command actually *executed* the Flow binary, as opposed
# to merely naming a path that contains "flow" as an argument to another program
# (`ls node_modules/.bin/flow`, `grep ... /repo/flow/...`, `readlink ./flow`).
# We split the command into pipeline/list segments, drop leading env-var
# assignments and exec wrappers (`timeout 60 ...`, `env ...`), and check the
# resulting program word. Bare `flow`/`./path/to/flow` or a package runner
# invoking flow (`npx flow`, `npx --yes flow check`, `yarn flow`) counts; bare
# `flow` with no subcommand still counts because the CLI defaults to a check, so
# requiring an explicit `check` would miss real runs.
_SEGMENT = re.compile(r"&&|\|\||[;\n|(){}]|\$\(")
_ENV_ASSIGN = re.compile(r"^\w+=")
_FLOW_WORD = re.compile(r"^(?:[\w./-]*/)?flow$|^\$FLOW(?:_BIN)?$")
_WRAPPERS = {"timeout", "env", "nohup", "time", "exec", "stdbuf", "xargs"}
_RUNNERS = {"npx", "yarn", "pnpm", "bunx", "bun"}
_RUNNER_VALUE_FLAGS = {
    "-p",
    "--cache",
    "--cwd",
    "--package",
    "--prefix",
    "--registry",
    "--shell",
    "--userconfig",
}
_RUNNER_VERBS = {"run", "exec", "x", "dlx"}

# A Flow type-check that finds errors prints its own diagnostic output: an
# "Error ----" banner per error and a "Found N errors" summary. Requiring this
# signature (in addition to a Flow command word) means a non-zero exit that
# actually came from a *chained* non-Flow command — `cat .flowconfig && ./flow
# version` failing on the missing file, or `... && ./flow check` short-circuited
# before Flow ran — is not miscounted, and neither are non-check subcommands
# (`flow version`, `flow ast`) or a check killed by `timeout` (no summary).
_FLOW_ERROR_OUTPUT = re.compile(r"Found \d+ error|Error -{5,}")


def _split_segment(segment):
    try:
        return shlex.split(segment)
    except ValueError:
        return segment.strip().split()


def _strip_runner_flags(tokens, index):
    while index < len(tokens):
        token = tokens[index]
        if token == "--":
            return index + 1
        if not token.startswith("-"):
            return index
        index += 1
        if "=" not in token and token in _RUNNER_VALUE_FLAGS:
            index += 1
    return index


def _runner_target_index(tokens, index):
    index = _strip_runner_flags(tokens, index + 1)
    if index < len(tokens) and tokens[index] in _RUNNER_VERBS:
        index = _strip_runner_flags(tokens, index + 1)
    return index if index < len(tokens) else None


def is_flow_invocation(command):
    # Keep this parser in sync with is_tsc_invocation in no_tsc.sh.
    # Normalize `${VAR}` to `$VAR` first: _SEGMENT treats `{`/`}` as delimiters,
    # so a braced binary reference (`${FLOW_BIN} check`) would otherwise be torn
    # apart before the program word is examined.
    command = re.sub(r"\$\{(\w+)\}", r"$\1", command)
    for segment in _SEGMENT.split(command):
        tokens = _split_segment(segment)
        i = 0
        # Strip leading env-assignments and exec wrappers, repeating so that a
        # wrapper's own assignments are consumed too (`env VAR=val ./flow check`).
        changed = True
        while changed:
            changed = False
            while i < len(tokens) and _ENV_ASSIGN.match(tokens[i]):
                i += 1
                changed = True
            while i < len(tokens) and tokens[i] in _WRAPPERS:
                i += 1
                changed = True
                # skip the wrapper's own option/duration args (e.g. `timeout 60`)
                while i < len(tokens) and re.match(r"^[-\d]", tokens[i]):
                    i += 1
        if i >= len(tokens):
            continue
        word = tokens[i]
        if _FLOW_WORD.match(word):
            return True
        if word in _RUNNERS:
            j = _runner_target_index(tokens, i)
            if j is None:
                continue
            target = tokens[j]
            if _FLOW_WORD.match(target) or (
                " " in target and is_flow_invocation(target)
            ):
                return True
    return False

# tool_use_id -> the Bash command that produced it, so an erroneous tool_result
# can be attributed back to the command that ran. The tool_use always precedes
# its tool_result in the trajectory, so a single forward pass is enough.
commands = {}

with open(traj_path) as f:
    for line in f:
        line = line.strip()
        if not line:
            continue
        try:
            msg = json.loads(line)
        except json.JSONDecodeError:
            continue

        msg_type = msg.get("type")
        content = msg.get("message", {}).get("content", [])
        if not isinstance(content, list):
            continue

        if msg_type == "assistant":
            for block in content:
                if not isinstance(block, dict) or block.get("type") != "tool_use":
                    continue
                block_id = block.get("id")
                tool_input = block.get("input", {})
                command = (
                    tool_input.get("command", "")
                    if isinstance(tool_input, dict)
                    else ""
                )
                if block_id is not None:
                    commands[block_id] = command if isinstance(command, str) else ""
        elif msg_type == "user":
            for block in content:
                if not isinstance(block, dict) or block.get("type") != "tool_result":
                    continue
                if block.get("is_error") is not True:
                    continue
                block_content = block.get("content", "")
                if not (
                    isinstance(block_content, str)
                    and block_content.startswith("Exit code")
                ):
                    continue
                command = commands.get(block.get("tool_use_id"), "")
                if is_flow_invocation(command) and _FLOW_ERROR_OUTPUT.search(
                    block_content
                ):
                    error_count += 1

print(error_count)
PYEOF
)

if [[ "$COUNT" -le "$FLOW_ERROR_THRESHOLD" ]]; then
  echo "{\"pass\": true, \"flow_error_count\": $COUNT, \"reason\": \"$COUNT erroneous flow calls (threshold: $FLOW_ERROR_THRESHOLD)\"}"
  exit 0
else
  echo "{\"pass\": false, \"flow_error_count\": $COUNT, \"reason\": \"$COUNT erroneous flow calls exceed threshold of $FLOW_ERROR_THRESHOLD\"}"
  exit 1
fi
