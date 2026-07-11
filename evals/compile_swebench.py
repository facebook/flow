#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""Compile eval directories into SWE-bench format JSONL.

Reads eval directories with the input/ideal structure and produces
a JSONL file (build/swebench/flow_evals.jsonl) with SWE-bench instances.

Usage:
  python3 compile_swebench.py [--evals-dir evals/] [--output-dir build/swebench/]

Each eval directory should have:
  prompt.md       - problem statement (what the model sees)
  config.json     - metadata, graders, version
  input/          - files for the base commit (buggy/empty state)
  ideal/          - files that differ from input (gold fix)
"""

import argparse
import difflib
import json
import os
import shlex
import sys
from datetime import datetime, timezone
from pathlib import Path

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
GRADERS_DIR = os.path.join(SCRIPT_DIR, "graders")

VALID_DIFFICULTIES = {"easy", "medium", "hard"}

# ---------------------------------------------------------------------------
# Baseline graders
#
# Every eval gets a category-appropriate baseline set prepended to the graders
# it declares in its own config.json. Centralizing this here — rather than
# repeating `no_extra_flow_errors` in each config — keeps the policy consistent
# and lets new evals inherit the right graders for their category automatically.
#
# The no_extra_flow_errors threshold encodes how many erroneous Flow calls are
# acceptable during the run:
#   error_fixing → 1 (the initial check that demonstrates the bug)
#   code-writing → 0 (the model should type-check on the first attempt)
# ---------------------------------------------------------------------------
_HYGIENE_GRADERS = [
    "file_modified",
    "flow_check",
    "no_flowfixme",
    "no_any",
    "no_commonjs",
]
_NO_EXTRA_STRICT = {"type": "no_extra_flow_errors", "threshold": 0}
_NO_EXTRA_ALLOW_INITIAL = {"type": "no_extra_flow_errors", "threshold": 1}

# no_tsc is a trajectory grader applied to every category: these are Flow evals,
# so invoking `tsc` is always wrong. no_extra_flow_errors intentionally counts
# only Flow's own errors (see its docstring), so foreign tooling is caught here.
_NO_TSC = "no_tsc"

# Baseline graders per eval category. Categories not listed fall back to
# _DEFAULT_BASELINE (a strict code-writing baseline).
_BASELINE_GRADERS = {
    "01_error_fixing": [*_HYGIENE_GRADERS, _NO_EXTRA_ALLOW_INITIAL, _NO_TSC],
    "02_unique_features": [*_HYGIENE_GRADERS, _NO_EXTRA_STRICT, _NO_TSC],
    "04_ts_to_flow": [*_HYGIENE_GRADERS, _NO_EXTRA_STRICT, _NO_TSC],
    "05_code_generation": [*_HYGIENE_GRADERS, _NO_EXTRA_STRICT, _NO_TSC],
    # Config/tooling evals edit .flowconfig, libdefs, or .flow declaration files
    # rather than always main.js, and some legitimately add suppressions or `any`
    # at a boundary — so only flow_check is universal, and each eval declares the
    # file_modified / AST / file_contains graders it needs. They also legitimately
    # iterate on flowconfig errors, so no_extra_flow_errors is omitted here (but
    # no_tsc still applies).
    "06_config_tooling": ["flow_check", _NO_TSC],
}
_DEFAULT_BASELINE = [*_HYGIENE_GRADERS, _NO_EXTRA_STRICT, _NO_TSC]

# Project-pattern evals let the model edit arbitrary context files, so the
# per-file hygiene graders (which target a fixed main.js) don't apply; each such
# eval lists the per-file graders it needs explicitly. Only flow_check plus the
# trajectory graders are universal.
_PROJECT_PATTERN_BASELINE = ["flow_check", _NO_EXTRA_STRICT, _NO_TSC]


def baseline_graders(category):
    """Return a fresh copy of the baseline grader list for an eval category."""
    return list(_BASELINE_GRADERS.get(category, _DEFAULT_BASELINE))


def grader_exists(name):
    """Check if a grader shell script exists in the graders/ directory."""
    return os.path.isfile(os.path.join(GRADERS_DIR, f"{name}.sh"))


def validate_config(config, eval_name):
    """Validate config.json schema. Returns a list of error messages."""
    errors = []

    # metadata is required
    metadata = config.get("metadata")
    if not isinstance(metadata, dict):
        errors.append("missing or invalid 'metadata' object")
    else:
        if not metadata.get("name"):
            errors.append("metadata.name is required")
        if not metadata.get("category"):
            errors.append("metadata.category is required")
        difficulty = metadata.get("difficulty")
        if difficulty is not None and difficulty not in VALID_DIFFICULTIES:
            errors.append(
                f"metadata.difficulty '{difficulty}' not in {VALID_DIFFICULTIES}"
            )

    # validate graders if present
    grading = config.get("grading", {})
    if not isinstance(grading, dict):
        errors.append(
            "'grading' must be an object (got {})".format(type(grading).__name__)
        )
        return errors
    for i, grader in enumerate(grading.get("graders", [])):
        if isinstance(grader, str):
            grader_name = grader
        elif isinstance(grader, dict):
            grader_name = grader.get("type", "")
            if not grader_name:
                errors.append(f"graders[{i}]: dict grader missing 'type'")
                continue
        else:
            errors.append(f"graders[{i}]: must be a string or dict")
            continue

        if grader_name == "no_extra_flow_errors":
            errors.append(
                f"graders[{i}]: 'no_extra_flow_errors' is applied automatically by "
                "the category baseline (see _BASELINE_GRADERS); remove the inline "
                "entry so the baseline stays the single source of truth"
            )
            continue

        if not grader_exists(grader_name):
            errors.append(
                f"graders[{i}]: grader script '{grader_name}.sh' not found in graders/"
            )

    return errors


def generate_grading_script(graders, eval_name):
    """Generate a TAP-format grading script from the grader list.

    Reads individual grader scripts from graders/ and composes them into
    a single TAP-format test script with pass/fail counting.

    Each per-file grader determines which files it applies to via the
    ``files`` key in its config dict. Defaults: ``no_any``, ``no_flowfixme``,
    and ``no_commonjs`` default to ``"*"`` (all .js files);
    ``ast_query`` and ``contains_ast_node_type`` default to ``"main.js"``.
    """
    lines = [
        "#!/bin/bash",
        "# Auto-generated grading script for Flow evals.",
        "# Outputs TAP (Test Anything Protocol) format.",
        f"# Eval: {eval_name}",
        "#",
        "# Usage: bash tests/run_graders.sh",
        "",
        "set -uo pipefail",
        "",
        'EVAL_DIR="$(cd "$(dirname "$0")/.." && pwd)"',
        'FLOW_BIN="${FLOW_BIN:-npx flow}"',
        'GRADERS_DIR="${GRADERS_DIR:-$(cd "$(dirname "$0")/../../graders" && pwd)}"',
        'EVAL_INPUT_DIR="${EVAL_INPUT_DIR:-$EVAL_DIR}"',
        "",
        "PASSED=0",
        "FAILED=0",
    ]

    test_num = 0

    for grader in graders:
        if isinstance(grader, str):
            grader_name = grader
            grader_args = {}
        elif isinstance(grader, dict):
            grader_name = grader.get("type", "")
            grader_args = grader
        else:
            continue

        test_num += 1
        # Build a display name for TAP output
        if isinstance(grader, dict) and grader.get("query"):
            tap_name = f"{grader_name}_{grader['query']}"
        elif isinstance(grader, dict) and grader.get("selector"):
            tap_name = f"{grader_name}_custom"
        else:
            tap_name = grader_name

        lines.append("")
        lines.append(f"# --- Test {test_num}: {tap_name} ---")

        if not grader_exists(grader_name):
            lines.extend(
                [
                    f'echo "not ok {test_num} {tap_name} # grader not found"',
                    "FAILED=$((FAILED + 1))",
                ]
            )
            continue

        # Build the grader command and determine if it runs per-file or per-dir.
        # Per-file graders take "$f" as first arg; per-dir take "$EVAL_DIR".
        per_file = False
        if grader_name == "flow_check":
            cmd = f'bash "$GRADERS_DIR/flow_check.sh" "$EVAL_DIR" "$FLOW_BIN"'
        elif grader_name == "file_modified":
            cmd = (
                f'bash "$GRADERS_DIR/file_modified.sh" "$f"'
                ' "$EVAL_INPUT_DIR/${f#$EVAL_DIR/}"'
            )
            per_file = True
            # file_modified.sh decides the result for missing files:
            # - original absent → pass (multi-file eval, new file created)
            # - file absent, original present → fail (agent didn't touch it)
            # So suppress the early-exit check and always call the grader.
            grader_args = dict(grader_args, _skip_missing_check=True)
        elif grader_name == "file_not_modified":
            cmd = (
                f'bash "$GRADERS_DIR/file_not_modified.sh" "$f"'
                ' "$EVAL_INPUT_DIR/${f#$EVAL_DIR/}"'
            )
            per_file = True
            # file_not_modified.sh handles a deleted file itself (that counts as
            # a modification → fail), so don't bail early on a missing file.
            grader_args = dict(grader_args, _skip_missing_check=True)
        elif grader_name == "no_flowfixme":
            cmd = f'bash "$GRADERS_DIR/no_flowfixme.sh" "$f"'
            per_file = True
        elif grader_name == "no_any":
            cmd = f'bash "$GRADERS_DIR/no_any.sh" "$f" "$FLOW_BIN"'
            per_file = True
        elif grader_name == "no_commonjs":
            cmd = f'bash "$GRADERS_DIR/no_commonjs.sh" "$f" "$FLOW_BIN"'
            per_file = True
        elif grader_name == "contains_ast_node_type":
            query = grader_args.get("query", "")
            negate_flag = " --negate" if grader_args.get("negate") else ""
            cmd = (
                f'bash "$GRADERS_DIR/contains_ast_node_type.sh" "$f" "$FLOW_BIN"'
                f' "{query}"{negate_flag}'
            )
            per_file = True
        elif grader_name == "ast_query":
            selector = grader_args.get("selector", "")
            negate_flag = " --negate" if grader_args.get("negate") else ""
            cmd = (
                f'bash "$GRADERS_DIR/ast_query.sh" "$f" "$FLOW_BIN"'
                f" '{selector}'{negate_flag}"
            )
            per_file = True
        elif grader_name == "file_contains":
            pattern = grader_args.get("pattern", "")
            cmd = f'bash "$GRADERS_DIR/file_contains.sh" "$f" {shlex.quote(pattern)}'
            per_file = True
        elif grader_name == "no_extra_flow_errors":
            # Default threshold=1 for Cat 1 error-fixing evals (one initial error expected).
            # Code-writing evals should set threshold=0 explicitly.
            threshold = grader_args.get("threshold", 1)
            cmd = f'FLOW_ERROR_THRESHOLD={threshold} bash "$GRADERS_DIR/no_extra_flow_errors.sh"'
        elif grader_name == "no_tsc":
            cmd = 'bash "$GRADERS_DIR/no_tsc.sh"'
        else:
            cmd = f'bash "$GRADERS_DIR/{grader_name}.sh" "$EVAL_DIR"'

        if per_file:
            # Determine which files this grader applies to.
            # Grader dicts can specify "files"; defaults depend on grader type.
            if isinstance(grader, dict) and "files" in grader:
                files_spec = grader["files"]
            elif grader_name in ("no_any", "no_flowfixme", "no_commonjs"):
                files_spec = "*"
            else:
                # ast_query, contains_ast_node_type default to main.js
                files_spec = "main.js"

            if files_spec == "*":
                file_loop = '"$EVAL_DIR"/*.js'
            elif isinstance(files_spec, list):
                if len(files_spec) == 0:
                    raise ValueError(
                        f"Grader '{grader_name}' in {eval_name} has empty files list"
                    )
                file_loop = " ".join(f'"$EVAL_DIR/{f}"' for f in files_spec)
            else:
                file_loop = f'"$EVAL_DIR/{files_spec}"'

            if files_spec == "*":
                # Glob: skip non-existent (no .js files is fine)
                missing_check = '  [ -f "$f" ] || continue'
            elif grader_args.get("_skip_missing_check"):
                # Grader handles missing files itself — don't bail early.
                missing_check = ""
            else:
                # Explicit file(s): fail if missing (catches typos)
                missing_check = (
                    '  if [ ! -f "$f" ]; then\n'
                    '    echo "Grader error: file not found: $f" >&2\n'
                    "    GRADER_PASS=false\n"
                    "    continue\n"
                    "  fi"
                )

            loop_body = [missing_check] if missing_check else []
            lines.extend(
                [
                    "GRADER_PASS=true",
                    f"for f in {file_loop}; do",
                    *loop_body,
                    f"  if ! {cmd} >/dev/null 2>&1; then",
                    "    GRADER_PASS=false",
                    "  fi",
                    "done",
                    'if [[ "$GRADER_PASS" == "true" ]]; then',
                    f'  echo "ok {test_num} {tap_name}"',
                    "  PASSED=$((PASSED + 1))",
                    "else",
                    f'  echo "not ok {test_num} {tap_name}"',
                    "  FAILED=$((FAILED + 1))",
                    "fi",
                ]
            )
        else:
            lines.extend(
                [
                    f"if {cmd} >/dev/null 2>&1; then",
                    f'  echo "ok {test_num} {tap_name}"',
                    "  PASSED=$((PASSED + 1))",
                    "else",
                    f'  echo "not ok {test_num} {tap_name}"',
                    "  FAILED=$((FAILED + 1))",
                    "fi",
                ]
            )

    total = test_num
    lines.extend(
        [
            "",
            "# --- TAP plan ---",
            f'echo "1..{total}"',
            "",
            'if [[ "$FAILED" -gt 0 ]]; then',
            "  exit 1",
            "fi",
            "exit 0",
            "",
        ]
    )

    return "\n".join(lines)


def compute_patch(input_dir, ideal_dir):
    """Compute unified diff between input/ and ideal/ using difflib.

    ideal/ is a sparse overlay — only files that differ from input/.
    We diff each ideal file against its input counterpart.
    """
    patches = []
    for root, _dirs, files in os.walk(ideal_dir):
        for fname in sorted(files):
            src_ideal = os.path.join(root, fname)
            rel = os.path.relpath(src_ideal, ideal_dir)
            src_input = os.path.join(input_dir, rel)

            input_lines = (
                Path(src_input).read_text().splitlines(keepends=True)
                if os.path.exists(src_input)
                else []
            )
            ideal_lines = Path(src_ideal).read_text().splitlines(keepends=True)

            diff = difflib.unified_diff(
                input_lines,
                ideal_lines,
                fromfile=f"a/{rel}",
                tofile=f"b/{rel}",
            )
            patch = "".join(diff)
            if patch:
                patches.append(patch)

    return "\n".join(patches) + "\n" if patches else ""


def is_project_pattern(eval_dir):
    """Return True if the directory uses the project-pattern format (context/ + candidate_prompts/)."""
    return os.path.isdir(os.path.join(eval_dir, "context")) and os.path.isdir(
        os.path.join(eval_dir, "candidate_prompts")
    )


def compile_project_pattern_evals(project_dir, flow_version):
    """Compile all candidate prompts in a project-pattern directory.

    Returns (instances, warnings, failed_count).
    """
    project_name = os.path.basename(project_dir)
    context_dir = os.path.join(project_dir, "context")
    prompts_dir = os.path.join(project_dir, "candidate_prompts")
    ideals_dir = os.path.join(project_dir, "ideals")

    instances = []
    warnings = []
    failed = 0

    for fname in sorted(os.listdir(prompts_dir)):
        if not fname.endswith(".md"):
            continue
        stem = fname[: -len(".md")]
        eval_name = f"{project_name}_{stem}"
        prompt_path = os.path.join(prompts_dir, fname)
        config_path = os.path.join(prompts_dir, f"{stem}.json")
        ideal_dir = os.path.join(ideals_dir, stem)

        missing = []
        if not os.path.isfile(config_path):
            missing.append(f"candidate_prompts/{stem}.json")
        if not os.path.isdir(ideal_dir):
            missing.append(f"ideals/{stem}/")
        if missing:
            warnings.append(f"  SKIP {eval_name} (missing {', '.join(missing)})")
            continue

        try:
            with open(prompt_path) as f:
                prompt = f.read().rstrip("\n")

            with open(config_path) as f:
                config = json.load(f)

            config_errors = validate_config(config, eval_name)
            if config_errors:
                for err in config_errors:
                    print(f"  ERROR {eval_name}/config.json: {err}", file=sys.stderr)
                raise ValueError(f"{eval_name}: config validation failed")

            grading = config.get("grading", {})
            version = config.get("version", flow_version)
            extra_graders = grading.get("graders", [])
            graders = list(_PROJECT_PATTERN_BASELINE) + extra_graders

            patch = compute_patch(context_dir, ideal_dir)
            if not patch.strip():
                warnings.append(
                    f"  WARNING: {eval_name} has no diff between context/ and ideals/{stem}/"
                )
                continue

            grading_script = generate_grading_script(graders, eval_name)

            fail_to_pass = []
            for g in graders:
                if isinstance(g, str):
                    fail_to_pass.append(g)
                elif isinstance(g, dict):
                    gtype = g.get("type", "")
                    label = g.get("query") or g.get("selector", "custom")
                    fail_to_pass.append(f"{gtype}_{label}")

            instances.append(
                {
                    "repo": "facebook/flow",
                    "instance_id": f"facebook__flow-{eval_name}",
                    "base_commit": "0000000",
                    "patch": patch,
                    "test_patch": grading_script,
                    "problem_statement": prompt,
                    "hints_text": "",
                    "created_at": datetime.now(timezone.utc).strftime(
                        "%Y-%m-%dT%H:%M:%SZ"
                    ),
                    "version": version,
                    "FAIL_TO_PASS": json.dumps(fail_to_pass),
                    "PASS_TO_PASS": json.dumps([]),
                    "environment_setup_commit": "0000000",
                    # Non-SWE-bench fields, used by run_swebench.py filters.
                    "category": config.get("metadata", {}).get("category", ""),
                    "tags": config.get("metadata", {}).get("tags", []),
                }
            )
        except (json.JSONDecodeError, ValueError) as e:
            print(f"  ERROR {eval_name}: {e}", file=sys.stderr)
            failed += 1

    return instances, warnings, failed


def compile_eval(eval_dir, flow_version="unknown"):
    """Compile a single eval directory into a SWE-bench instance."""
    eval_name = os.path.basename(eval_dir)
    prompt_path = os.path.join(eval_dir, "prompt.md")
    config_path = os.path.join(eval_dir, "config.json")
    input_dir = os.path.join(eval_dir, "input")
    ideal_dir = os.path.join(eval_dir, "ideal")

    # Check required files/dirs
    missing = []
    for path, label in [
        (prompt_path, "prompt.md"),
        (config_path, "config.json"),
        (input_dir, "input/"),
        (ideal_dir, "ideal/"),
    ]:
        if not os.path.exists(path):
            missing.append(label)

    if missing:
        print(
            f"  SKIP {eval_name}/ (missing {', '.join(missing)})",
            file=sys.stderr,
        )
        return None

    # Read prompt
    with open(prompt_path) as f:
        prompt = f.read().rstrip("\n")

    # Read and validate config
    with open(config_path) as f:
        config = json.load(f)

    config_errors = validate_config(config, eval_name)
    if config_errors:
        for err in config_errors:
            print(f"  ERROR {eval_name}/config.json: {err}", file=sys.stderr)
        raise ValueError(f"{eval_name}: config validation failed")

    grading = config.get("grading", {})
    version = config.get("version", flow_version)
    category = os.path.basename(os.path.dirname(os.path.abspath(eval_dir)))
    default_graders = baseline_graders(category)
    extra_graders = grading.get("graders", [])
    graders = default_graders + extra_graders

    # Compute patch (diff between input/ and ideal/)
    patch = compute_patch(input_dir, ideal_dir)

    if not patch.strip():
        print(
            f"  WARNING: {eval_name}/ has no diff between input/ and ideal/",
            file=sys.stderr,
        )
        return None

    # Generate grading script
    grading_script = generate_grading_script(graders, eval_name)

    # Build FAIL_TO_PASS from grader names
    fail_to_pass = []
    for g in graders:
        if isinstance(g, str):
            fail_to_pass.append(g)
        elif isinstance(g, dict):
            gtype = g.get("type", "")
            label = g.get("query") or g.get("selector", "custom")
            fail_to_pass.append(f"{gtype}_{label}")

    instance_id = f"facebook__flow-{eval_name}"

    return {
        "repo": "facebook/flow",
        "instance_id": instance_id,
        "base_commit": "0000000",
        "patch": patch,
        "test_patch": grading_script,
        "problem_statement": prompt,
        "hints_text": "",
        "created_at": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "version": version,
        "FAIL_TO_PASS": json.dumps(fail_to_pass),
        "PASS_TO_PASS": json.dumps([]),
        "environment_setup_commit": "0000000",
        # Non-SWE-bench fields, used by run_swebench.py --category/--tag filters.
        "category": config.get("metadata", {}).get("category", ""),
        "tags": config.get("metadata", {}).get("tags", []),
    }


def main():
    parser = argparse.ArgumentParser(
        description="Compile eval directories into SWE-bench JSONL"
    )
    parser.add_argument(
        "--evals-dir",
        default="evals",
        help="Source directory with eval subdirectories (default: evals/)",
    )
    parser.add_argument(
        "--output-dir",
        default="build/swebench",
        help="Output directory for SWE-bench JSONL (default: build/swebench/)",
    )
    parser.add_argument(
        "--flow-bin",
        default=os.path.join(SCRIPT_DIR, "node_modules", ".bin", "flow"),
        help="Path to Flow binary",
    )
    parser.add_argument(
        "--category",
        default=None,
        help="Only compile evals from this category (e.g., 02_unique_features)",
    )
    args = parser.parse_args()

    evals_dir = os.path.join(SCRIPT_DIR, args.evals_dir)
    output_dir = os.path.join(SCRIPT_DIR, args.output_dir)

    if not os.path.isdir(evals_dir):
        print(f"Error: {evals_dir} is not a directory", file=sys.stderr)
        sys.exit(1)

    flow_version = args.flow_bin

    # Find category directories
    categories = sorted(
        d
        for d in os.listdir(evals_dir)
        if os.path.isdir(os.path.join(evals_dir, d)) and d[0:2].isdigit()
    )

    if args.category:
        if args.category not in categories:
            print(
                f"Error: category '{args.category}' not found in {evals_dir}",
                file=sys.stderr,
            )
            sys.exit(1)
        categories = [args.category]

    if not categories:
        print(f"No category directories found in {evals_dir}", file=sys.stderr)
        sys.exit(1)

    os.makedirs(output_dir, exist_ok=True)

    all_instances = []
    total = 0
    skipped = 0
    failed = 0
    warnings = []

    for category in categories:
        category_dir = os.path.join(evals_dir, category)
        eval_dirs = sorted(
            d
            for d in os.listdir(category_dir)
            if os.path.isdir(os.path.join(category_dir, d))
        )

        for eval_name in eval_dirs:
            eval_dir = os.path.join(category_dir, eval_name)

            # Project-pattern format: shared context/ + per-prompt candidate_prompts/
            if is_project_pattern(eval_dir):
                instances, proj_warnings, proj_failed = compile_project_pattern_evals(
                    eval_dir, flow_version
                )
                all_instances.extend(instances)
                warnings.extend(proj_warnings)
                total += len(instances)
                failed += proj_failed
                continue

            # Only compile evals that have the new input/ideal structure
            if not os.path.isdir(os.path.join(eval_dir, "input")):
                warnings.append(f"  SKIP {eval_name}/ (no input/ dir, old format)")
                skipped += 1
                continue

            try:
                instance = compile_eval(eval_dir, flow_version)
            except (json.JSONDecodeError, ValueError) as e:
                print(f"  ERROR {eval_name}: {e}", file=sys.stderr)
                failed += 1
                continue
            if instance is None:
                skipped += 1
                continue

            all_instances.append(instance)
            total += 1

    # Write JSONL
    output_path = os.path.join(output_dir, "flow_evals.jsonl")
    with open(output_path, "w") as f:
        for instance in all_instances:
            f.write(json.dumps(instance) + "\n")

    for w in warnings:
        print(w)
    suffix = []
    if skipped:
        suffix.append(f"{skipped} skipped")
    if failed:
        suffix.append(f"{failed} failed")
    extra = f" ({', '.join(suffix)})" if suffix else ""
    print(f"Compiled {total} evals to {output_path}{extra}")

    if failed > 0:
        sys.exit(1)


if __name__ == "__main__":
    main()
