#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Grader: no_tsc (trajectory grader)
# Fails when the agent invoked the TypeScript compiler (`tsc`) anywhere in the
# trajectory. These are Flow evals: reaching for `tsc` (e.g. `tsc`, `npx tsc`,
# `./node_modules/.bin/tsc`) means the model is treating the task as TypeScript
# instead of Flow, which is wrong regardless of whether the command succeeded.
#
# Detection is by command, not exit code, so even a `tsc` run that exits 0 fails.
#
# When no trajectory exists (dry-run mode), this grader always passes.
#
# Usage: no_tsc.sh
# Env:   TRAJECTORY_PATH - path to the trajectory JSONL (optional)
# Exit code: 0 = pass (no tsc use), 1 = fail (tsc used)

set -euo pipefail

TRAJECTORY_PATH="${TRAJECTORY_PATH:-}"

if [[ -z "$TRAJECTORY_PATH" || ! -f "$TRAJECTORY_PATH" ]]; then
  echo '{"pass": true, "reason": "no trajectory available (dry-run or missing)"}'
  exit 0
fi

FOUND=$(python3 - "$TRAJECTORY_PATH" <<'PYEOF'
import json
import re
import shlex
import sys

traj_path = sys.argv[1]

# Decide whether a Bash command actually *ran* the TypeScript compiler, as
# opposed to merely naming `tsc` as an argument to another program
# (`which ts-node tsc node`, `find / -iname tsc`, `ls node_modules/.bin/tsc`).
# We split the command into pipeline/list segments, drop leading env-var
# assignments and exec wrappers (`timeout 60 ...`, `env ...`), and check the
# resulting program word. Only `tsc`/`./path/to/tsc` or a package runner
# invoking tsc (`npx tsc`, `npx --yes tsc`, `yarn tsc`) counts. A probe like
# `which tsc` has program word `which`, so it does not.
_SEGMENT = re.compile(r"&&|\|\||[;\n|(){}]|\$\(")
_ENV_ASSIGN = re.compile(r"^\w+=")
_TSC_WORD = re.compile(r"^(?:[\w./-]*/)?tsc$")
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


def is_tsc_invocation(command):
    # Keep this parser in sync with is_flow_invocation in no_extra_flow_errors.sh.
    # Normalize `${VAR}` to `$VAR` first: _SEGMENT treats `{`/`}` as delimiters,
    # so a braced binary reference (`${TSC_BIN} --version`) would otherwise be
    # torn apart before the program word is examined.
    command = re.sub(r"\$\{(\w+)\}", r"$\1", command)
    for segment in _SEGMENT.split(command):
        tokens = _split_segment(segment)
        i = 0
        # Strip leading env-assignments and exec wrappers, repeating so that a
        # wrapper's own assignments are consumed too (`env VAR=val tsc`).
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
        if _TSC_WORD.match(word):
            return True
        if word in _RUNNERS:
            j = _runner_target_index(tokens, i)
            if j is None:
                continue
            target = tokens[j]
            if _TSC_WORD.match(target) or (
                " " in target and is_tsc_invocation(target)
            ):
                return True
    return False


found = False
with open(traj_path) as f:
    for line in f:
        line = line.strip()
        if not line:
            continue
        try:
            msg = json.loads(line)
        except json.JSONDecodeError:
            continue

        if msg.get("type") != "assistant":
            continue

        content = msg.get("message", {}).get("content", [])
        if not isinstance(content, list):
            continue

        for block in content:
            if not isinstance(block, dict) or block.get("type") != "tool_use":
                continue
            tool_input = block.get("input", {})
            command = (
                tool_input.get("command", "") if isinstance(tool_input, dict) else ""
            )
            if isinstance(command, str) and is_tsc_invocation(command):
                found = True
                break
        if found:
            break

print("FOUND" if found else "")
PYEOF
)

if [[ -z "$FOUND" ]]; then
  echo '{"pass": true, "reason": "no tsc invocation in trajectory"}'
  exit 0
else
  echo '{"pass": false, "reason": "tsc invoked in trajectory (use Flow, not tsc)"}'
  exit 1
fi
