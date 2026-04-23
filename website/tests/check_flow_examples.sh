#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Snapshot test for website flow-check code blocks.
#
# Extracts every ```flow-check block from website/docs/, runs each through
# Flow's check-contents, and compares the combined output against a snapshot.
#
# Usage:
#   ./website/tests/check_flow_examples.sh FLOW_BINARY          # run test
#   ./website/tests/check_flow_examples.sh -r FLOW_BINARY       # re-record snapshot
#
# Examples:
#   ./website/tests/check_flow_examples.sh facebook/flowd
#   ./website/tests/check_flow_examples.sh -r $(buck build //flow:flow --show-full-output | awk '{print $2}')
#
# Buck:
#   buck test //flow:website_docs_flow_check

set -e -o pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

record=0
if [[ "$1" == "-r" ]]; then
  record=1
  shift
fi

FLOW="$1"
if [[ -z "$FLOW" ]]; then
  echo "Usage: $0 [-r] FLOW_BINARY [WEBSITE_DIR]" >&2
  exit 1
fi

# Optional second arg overrides the website directory (used by buck).
if [[ -n "$2" ]]; then
  WEBSITE_DIR="$(cd "$2" && pwd)"
else
  WEBSITE_DIR="$SCRIPT_DIR/.."
fi

DOCS_DIR="$WEBSITE_DIR/docs"
EXP_FILE="$SCRIPT_DIR/docs_flow_check.exp"

# Resolve FLOW to absolute path. If it doesn't contain a slash, look it up via
# buck build (for convenience names like "facebook/flowd").
if [[ "$FLOW" != /* ]]; then
  if [[ "$FLOW" == */* && ! -x "$FLOW" ]]; then
    # Relative path like facebook/flowd – resolve via buck
    FLOW=$(buck build "//flow:${FLOW##*/}" --show-full-output 2>/dev/null | awk '{print $2}')
    if [[ -z "$FLOW" || ! -x "$FLOW" ]]; then
      echo "Could not resolve Flow binary" >&2
      exit 1
    fi
  elif [[ ! -x "$FLOW" ]]; then
    FLOW="$(which "$FLOW" 2>/dev/null || true)"
  fi
fi

if ! "$FLOW" version >/dev/null 2>&1; then
  echo "Flow binary not working: $FLOW" >&2
  echo "Did you build it first? Try: buck build //flow" >&2
  exit 1
fi

# --- Set up temp environment ---
WORK_DIR=$(mktemp -d /tmp/flow_doc_test.XXXXXX)
trap '$FLOW stop "$WORK_DIR" 2>/dev/null; rm -rf "$WORK_DIR"' EXIT

# Create .flowconfig from the snippet config, but remove the PROJECT_ROOT
# ignore so example files are checked.
sed '/<PROJECT_ROOT>\/\.\*/d' "$WEBSITE_DIR/.flowconfig.snippets" > "$WORK_DIR/.flowconfig"

# Copy flow-typed stubs
cp -r "$WEBSITE_DIR/flow-typed" "$WORK_DIR/flow-typed"

# --- Extract and check ---
OUT_FILE="$WORK_DIR/output.out"

python3 - "$FLOW" "$DOCS_DIR" "$WORK_DIR" "$OUT_FILE" << 'PYTHON_SCRIPT'
import re, subprocess, sys, os

FLOW_BIN = sys.argv[1]
DOCS_DIR = sys.argv[2]
WORK_DIR = sys.argv[3]
OUT_FILE = sys.argv[4]

# Start the Flow server once
subprocess.run(
    [FLOW_BIN, 'start', WORK_DIR, '--wait'],
    capture_output=True, timeout=120
)

# Collect all doc files with flow-check blocks, sorted for determinism
doc_files = []
for root, dirs, files in os.walk(DOCS_DIR):
    dirs.sort()
    for f in sorted(files):
        if f.endswith('.md'):
            path = os.path.join(root, f)
            with open(path) as fh:
                if 'flow-check' in fh.read():
                    doc_files.append(path)

doc_files.sort()

output_lines = []
validation_failures = []
block_count = 0

for filepath in doc_files:
    with open(filepath) as f:
        content = f.read()

    rel_path = os.path.relpath(filepath, DOCS_DIR)
    pattern = r'```(?:js|jsx)\s+flow-check\s*\n(.*?)```'
    matches = list(re.finditer(pattern, content, re.DOTALL))

    for i, match in enumerate(matches):
        block_count += 1
        code = match.group(1)

        # Write to temp file with @flow header
        example_file = os.path.join(WORK_DIR, 'example.js')
        with open(example_file, 'w') as f:
            f.write('// @flow\n')
            f.write(code)

        try:
            with open(example_file) as sf:
                proc = subprocess.run(
                    [FLOW_BIN, 'check-contents', '--root', WORK_DIR, '--strip-root'],
                    stdin=sf,
                    capture_output=True,
                    text=True,
                    timeout=60
                )
        except subprocess.TimeoutExpired:
            output_lines.append(f"=== {rel_path} block {i+1} ===")
            output_lines.append("TIMEOUT")
            output_lines.append("")
            continue

        result = (proc.stdout + proc.stderr).strip()

        # Filter out server startup noise
        filtered = []
        for line in result.split('\n'):
            s = line.strip()
            if s.startswith('Spawned') or s.startswith('Logs will') or \
               s.startswith('Monitor logs') or s.startswith('Started a new') or \
               s.startswith('Launching Flow') or s.startswith('Please wait') or \
               s.startswith('Trying to connect'):
                continue
            filtered.append(line)
        result = '\n'.join(filtered).strip()

        if not result:
            result = "No errors!"

        output_lines.append(f"=== {rel_path} block {i+1} ===")
        output_lines.append(result)
        output_lines.append("")

        # Validate error annotations match actual errors
        source_lines = ('// @flow\n' + code).splitlines()
        error_lines = set()
        for err_match in re.finditer(
            r'^Error\s+-+\s+-:(\d+):\d+', result, re.MULTILINE
        ):
            error_lines.add(int(err_match.group(1)))

        HAS_ERROR_ANNOTATION = r'(?://|/\*).*\berror\b'
        IS_EXCLUDED = r'(?:\bno\b.*\berror\b|flowlint)'

        # Check 1: each error location should have a // error annotation
        for line_no in error_lines:
            if 0 < line_no <= len(source_lines):
                src_line = source_lines[line_no - 1]
                if not re.search(HAS_ERROR_ANNOTATION, src_line, re.IGNORECASE):
                    validation_failures.append(
                        f"{rel_path} block {i+1} line {line_no}: "
                        f"missing // error: {src_line.strip()}"
                    )

        # Check 2: each // error annotation should have a corresponding error
        for line_no, src_line in enumerate(source_lines, 1):
            if not re.search(HAS_ERROR_ANNOTATION, src_line, re.IGNORECASE):
                continue
            if re.search(IS_EXCLUDED, src_line, re.IGNORECASE):
                continue
            if src_line.lstrip().startswith(('//', '/*')):
                continue
            if line_no not in error_lines:
                validation_failures.append(
                    f"{rel_path} block {i+1} line {line_no}: "
                    f"has // error but no error reported: {src_line.strip()}"
                )

# Stop the Flow server
subprocess.run([FLOW_BIN, 'stop', WORK_DIR], capture_output=True)

# Write output
with open(OUT_FILE, 'w') as f:
    f.write('\n'.join(output_lines) + '\n')

# Write validation failures
validation_file = os.path.join(WORK_DIR, 'validation_failures.txt')
with open(validation_file, 'w') as f:
    for failure in validation_failures:
        f.write(failure + '\n')

print(f"Checked {block_count} flow-check blocks from {len(doc_files)} files", file=sys.stderr)
PYTHON_SCRIPT

# --- Validate error annotations ---
VALIDATION_FILE="$WORK_DIR/validation_failures.txt"
if [[ -s "$VALIDATION_FILE" ]]; then
  echo "Error annotation validation failures:" >&2
  cat "$VALIDATION_FILE" >&2
  echo "" >&2
  echo "Each line that Flow reports an error on should have a '// error' comment," >&2
  echo "and each '// error' comment should correspond to an actual error." >&2
  exit 1
fi

# --- Compare or record ---
if [[ "$record" -eq 1 ]]; then
  cp "$OUT_FILE" "$EXP_FILE"
  echo "Recorded snapshot to $(basename "$EXP_FILE")"
  exit 0
fi

if [[ ! -f "$EXP_FILE" ]]; then
  echo "No snapshot file found. Run with -r to create one:" >&2
  echo "  $0 -r FLOW_BINARY" >&2
  cp "$OUT_FILE" "$(dirname "$EXP_FILE")/docs_flow_check.out"
  echo "Output saved to website/tests/docs_flow_check.out for inspection." >&2
  exit 1
fi

if diff -u --strip-trailing-cr \
    --label "expected" --label "actual" \
    "$EXP_FILE" "$OUT_FILE"; then
  echo "All flow-check examples match snapshot."
  exit 0
else
  echo ""
  echo "Flow-check examples differ from snapshot."
  echo "If the changes are intentional, re-record with:"
  echo "  $0 -r FLOW_BINARY"
  exit 1
fi
