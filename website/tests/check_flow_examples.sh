#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Snapshot test for website flow-check code blocks.
#
# Extracts every ```flow-check block from website/docs/ (*.md) and
# website/src/pages/ (*.mdx), writes each as a separate file into a temp Flow
# root, runs a single `flow check`, and compares the per-doc snapshots under
# website/tests/snapshots/. Pages snapshots live under pages/ to avoid
# colliding with doc paths.
#
# Usage:
#   ./website/tests/check_flow_examples.sh FLOW_BINARY          # run test
#   ./website/tests/check_flow_examples.sh -r FLOW_BINARY       # re-record snapshot
#
# Examples:
#   ./website/tests/check_flow_examples.sh $(buck build //flow/rust_port/crates/flow_cli:flow_cli --show-full-output | awk '{print $2}')
#   ./website/tests/check_flow_examples.sh -r $(buck build //flow/rust_port/crates/flow_cli:flow_cli --show-full-output | awk '{print $2}')
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
PAGES_DIR="$WEBSITE_DIR/src/pages"
SNAPSHOT_DIR="$SCRIPT_DIR/snapshots"

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
  echo "Did you build it first? Try: buck build //flow/rust_port/crates/flow_cli:flow_cli" >&2
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

# Snapshots produced by this run go here; we compare against $SNAPSHOT_DIR.
GENERATED_DIR="$WORK_DIR/generated_snapshots"
mkdir -p "$GENERATED_DIR"

# --- Extract blocks, run flow once, render per-doc snapshots ---
python3 - "$FLOW" "$DOCS_DIR" "$PAGES_DIR" "$WORK_DIR" "$GENERATED_DIR" << 'PYTHON_SCRIPT'
import os, re, subprocess, sys

FLOW_BIN = sys.argv[1]
DOCS_DIR = sys.argv[2]
PAGES_DIR = sys.argv[3]
WORK_DIR = sys.argv[4]
GENERATED_DIR = sys.argv[5]

SNIPPETS_DIR = os.path.join(WORK_DIR, 'snippets')
os.makedirs(SNIPPETS_DIR, exist_ok=True)

# Source roots to scan. Each tuple is:
#   (root_dir, accepted_extensions, snapshot_prefix)
# The snapshot prefix keeps page snapshots from colliding with doc paths.
SOURCES = [
    (DOCS_DIR, ('.md',), ''),
    (PAGES_DIR, ('.mdx',), 'pages'),
]

# 1. Collect doc/page files containing flow-check blocks (sorted for
#    determinism). doc_files is a list of (filepath, display_rel_path) where
#    display_rel_path is what appears in snapshot output and on disk.
doc_files = []
for root_dir, exts, prefix in SOURCES:
    if not os.path.isdir(root_dir):
        continue
    for root, dirs, files in os.walk(root_dir):
        dirs.sort()
        for f in sorted(files):
            if not f.endswith(exts):
                continue
            path = os.path.join(root, f)
            with open(path) as fh:
                if 'flow-check' in fh.read():
                    rel = os.path.relpath(path, root_dir)
                    display = os.path.join(prefix, rel) if prefix else rel
                    doc_files.append((path, display))
doc_files.sort(key=lambda pair: pair[1])

# 1b. Reject malformed flow-check fences. The extractor below only matches the
#     canonical ```js flow-check / ```jsx flow-check opener; a fence that
#     mentions "flow-check" any other way (a stray space as in "``` js
#     flow-check", a wrong/missing language, or "```flow-check") is silently
#     skipped, leaving its examples unchecked. Fail eagerly on the first such
#     fence, before the (slow) Flow server starts, so they can't rot unverified.
CANONICAL_FENCE_RE = re.compile(r'```(?:js|jsx)\s+flow-check\b')
for filepath, rel_path in doc_files:
    with open(filepath) as fh:
        for line_no, line in enumerate(fh, 1):
            if not line.lstrip().startswith('```'):
                continue
            if 'flow-check' not in line:
                continue
            if not CANONICAL_FENCE_RE.search(line):
                sys.exit(
                    f'Malformed flow-check code fence at {rel_path}:{line_no}:\n'
                    f'  {line.strip()}\n\n'
                    "A code fence that mentions 'flow-check' must be written exactly as\n"
                    '  ```js flow-check        (or ```jsx flow-check)\n'
                    "optionally followed by options such as 'first-error'. Any other\n"
                    'form (stray space, wrong language, missing language) is silently\n'
                    'skipped by this snapshot test, leaving the example unverified.'
                )

# 2. Extract blocks and write each as its own file in WORK_DIR/snippets/.
#    Map: display_rel_path -> [(block_idx, snippet_filename, code)]
BLOCK_RE = re.compile(
    r'```(?:js|jsx)\s+flow-check\b([^\n]*)\n(.*?)```', re.DOTALL
)
blocks_by_doc = {}
for filepath, rel_path in doc_files:
    with open(filepath) as f:
        content = f.read()
    matches = list(BLOCK_RE.finditer(content))
    if not matches:
        continue
    stem = os.path.splitext(rel_path)[0]
    safe = re.sub(r'[^a-zA-Z0-9]', '_', stem)
    blocks_by_doc[rel_path] = []
    for i, m in enumerate(matches):
        idx = i + 1
        opts = set(m.group(1).split())
        code = m.group(2)
        snippet_name = f'{safe}__{idx:03d}.js'
        with open(os.path.join(SNIPPETS_DIR, snippet_name), 'w') as f:
            f.write('// @flow\n')
            f.write(code)
        blocks_by_doc[rel_path].append((idx, snippet_name, code, opts))

# 3. Start server (with snippets already on disk so the initial check covers
#    everything) and pull all errors in one shot.
subprocess.run(
    [FLOW_BIN, 'start', WORK_DIR, '--wait'],
    capture_output=True, timeout=300,
)
proc = subprocess.run(
    [FLOW_BIN, 'status', '--strip-root', '--show-all-errors',
     '--message-width', '120', WORK_DIR],
    capture_output=True, text=True, timeout=300,
)
subprocess.run([FLOW_BIN, 'stop', WORK_DIR], capture_output=True)

raw_output = proc.stdout

# 4. Split output into per-error chunks and bucket by snippet file.
#    Each chunk starts at "Error -+ snippets/<name>:<line>:<col>" and runs
#    until the next "Error " line or the "Found N errors" trailer.
ERROR_CHUNK_RE = re.compile(
    r'^(Error\s+-+\s+snippets/([^:]+):\d+:\d+.*?)'
    r'(?=^Error\s+-+\s+snippets/|^Found\s+\d+\s+error|\Z)',
    re.MULTILINE | re.DOTALL,
)
errors_by_snippet = {}
for m in ERROR_CHUNK_RE.finditer(raw_output):
    chunk = m.group(1).rstrip()
    snippet_name = m.group(2)
    errors_by_snippet.setdefault(snippet_name, []).append(chunk)

# 5. Render per-doc snapshots into GENERATED_DIR; validate // error markers.
HAS_ERROR_ANNOTATION = re.compile(r'(?://|/\*).*\berror\b', re.IGNORECASE)
IS_EXCLUDED = re.compile(r'(?:\bno\b.*\berror\b|flowlint)', re.IGNORECASE)
SNIPPET_PATH_RE = re.compile(r'snippets/[A-Za-z0-9_]+\.js')
ERROR_LINE_RE = re.compile(r'^Error\s+-+\s+-:(\d+):\d+', re.MULTILINE)

validation_failures = []
block_count = 0

for rel_path in sorted(blocks_by_doc):
    out_lines = []
    for idx, snippet_name, code, opts in blocks_by_doc[rel_path]:
        block_count += 1
        chunks = errors_by_snippet.get(snippet_name, [])
        # Normalize: rewrite snippet paths to "-" so the snapshot is stable
        # regardless of the chosen on-disk filename.
        normalized = [SNIPPET_PATH_RE.sub('-', c) for c in chunks]

        out_lines.append(f'=== block {idx} ===')
        if not normalized:
            out_lines.append('No errors!')
        else:
            out_lines.append('\n\n'.join(normalized))
            out_lines.append('')
            out_lines.append('')
            n = len(normalized)
            out_lines.append(f"Found {n} error{'s' if n != 1 else ''}")
        out_lines.append('')

        # Validation: each Error location should have a // error annotation,
        # and each // error annotation should have a corresponding error.
        source_lines = ('// @flow\n' + code).splitlines()
        error_line_nos = set()
        for chunk in normalized:
            for em in ERROR_LINE_RE.finditer(chunk):
                error_line_nos.add(int(em.group(1)))

        # `first-error` blocks only render the first error in the UI, so we
        # only require the first error line to carry an annotation.
        required_lines = (
            {min(error_line_nos)}
            if 'first-error' in opts and error_line_nos
            else error_line_nos
        )
        for line_no in required_lines:
            if 0 < line_no <= len(source_lines):
                src_line = source_lines[line_no - 1]
                if not HAS_ERROR_ANNOTATION.search(src_line):
                    validation_failures.append(
                        f"{rel_path} block {idx} line {line_no}: "
                        f"missing // error: {src_line.strip()}"
                    )

        for line_no, src_line in enumerate(source_lines, 1):
            if not HAS_ERROR_ANNOTATION.search(src_line):
                continue
            if IS_EXCLUDED.search(src_line):
                continue
            if src_line.lstrip().startswith(('//', '/*')):
                continue
            if line_no not in error_line_nos:
                validation_failures.append(
                    f"{rel_path} block {idx} line {line_no}: "
                    f"has // error but no error reported: {src_line.strip()}"
                )

    # Write per-doc snapshot (path mirrors the doc/page path, with the
    # source extension swapped for .exp).
    snap_rel = os.path.splitext(rel_path)[0] + '.exp'
    snap_path = os.path.join(GENERATED_DIR, snap_rel)
    os.makedirs(os.path.dirname(snap_path), exist_ok=True)
    with open(snap_path, 'w') as f:
        f.write('\n'.join(out_lines).rstrip() + '\n')

with open(os.path.join(WORK_DIR, 'validation_failures.txt'), 'w') as f:
    for fail in validation_failures:
        f.write(fail + '\n')

print(
    f"Checked {block_count} flow-check blocks from {len(blocks_by_doc)} files",
    file=sys.stderr,
)
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
  # Wipe the snapshot dir first so deleted docs don't leave stale snapshots
  # behind.
  rm -rf "$SNAPSHOT_DIR"
  mkdir -p "$SNAPSHOT_DIR"
  cp -r "$GENERATED_DIR/." "$SNAPSHOT_DIR/"
  echo "Recorded snapshots to $(basename "$SNAPSHOT_DIR")/"
  exit 0
fi

if [[ ! -d "$SNAPSHOT_DIR" ]]; then
  echo "No snapshot directory found. Run with -r to create one:" >&2
  echo "  $0 -r FLOW_BINARY" >&2
  exit 1
fi

# `diff -r` reports both content differences AND files present on only one
# side, so a deleted doc (stale snapshot) or a new doc (missing snapshot)
# both fail the test.
if diff -r --strip-trailing-cr "$SNAPSHOT_DIR" "$GENERATED_DIR"; then
  echo "All flow-check examples match snapshots."
  exit 0
else
  echo ""
  echo "Flow-check examples differ from snapshots."
  echo "If the changes are intentional, re-record with:"
  echo "  $0 -r FLOW_BINARY"
  exit 1
fi
