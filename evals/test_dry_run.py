# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
Dry-run test for Flow evals: applies gold patches and checks all graders pass.

Each eval in the category is a separate test case so CI reports per-eval
pass/fail rather than a single opaque FATAL.

Buck passes:
  FLOW_BIN      — path to the Flow binary (via $(location //flow:flow))
  TEST_CATEGORY — category directory name (e.g. "04_ts_to_flow")
"""

import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

FLOW_BIN = os.environ.get("FLOW_BIN", "flow")
CATEGORY = os.environ.get("TEST_CATEGORY", "")

# SCRIPT_DIR must be a real filesystem path so we can run scripts as subprocesses.
# In Buck par mode, __file__ may be a path inside a zip archive. Resolve it to
# a real path, falling back to CWD-relative lookup if resolution gives a
# non-existent directory (which happens when running inside a par).
_here = Path(__file__).resolve().parent
if not (_here / "compile_swebench.py").exists():
    # Try importlib.resources to extract to a real directory
    try:
        import importlib.resources

        _pkg = importlib.resources.files(__package__ or __name__.rsplit(".", 1)[0])
        _here = Path(str(_pkg))
    except Exception:
        pass

SCRIPT_DIR = _here


def _compile_evals(output_dir):
    """Compile evals to JSONL and return the list of eval names."""
    compile_script = SCRIPT_DIR / "compile_swebench.py"
    cmd = [sys.executable, str(compile_script), "--output-dir", output_dir]
    if CATEGORY:
        cmd += ["--category", CATEGORY]
    result = subprocess.run(cmd, capture_output=True, text=True, cwd=str(SCRIPT_DIR))
    if result.returncode != 0:
        raise RuntimeError(
            f"compile_swebench.py failed (cwd={SCRIPT_DIR}, "
            f"script_exists={compile_script.exists()}):\n{result.stderr}"
        )

    names = []
    with open(os.path.join(output_dir, "flow_evals.jsonl")) as f:
        for line in f:
            inst = json.loads(line)
            names.append(inst["instance_id"].replace("facebook__flow-", ""))
    return names


# Compile at module load so test discovery can enumerate the methods.
# Errors are captured rather than raised so that a single test_setup_failed
# method surfaces the problem instead of silently producing 0 tests.
_tmpdir = tempfile.mkdtemp(prefix="flow-eval-dry-run-")
_eval_names = []
_setup_error = None

try:
    _eval_names = _compile_evals(_tmpdir)
except Exception as exc:
    _setup_error = f"{type(exc).__name__}: {exc}\nSCRIPT_DIR={SCRIPT_DIR}"


class EvalDryRunTests(unittest.TestCase):
    """One test_<eval_name> method per eval, populated below."""


# Always present: surfaces setup failures as a proper test FAIL rather than
# 0-tests-ran, and validates that the class itself is discovered by pytest.
def _test_setup(self):
    if _setup_error:
        self.fail(f"Eval compile/setup failed:\n{_setup_error}")


EvalDryRunTests.test_setup = _test_setup  # pyre-ignore[16]


def _make_test(eval_name):
    def test_method(self):
        result_file = os.path.join(_tmpdir, f"result_{eval_name}.json")
        subprocess.run(
            [
                sys.executable,
                str(SCRIPT_DIR / "run_swebench.py"),
                "--dry-run",
                "--flow-bin",
                FLOW_BIN,
                "--jsonl",
                os.path.join(_tmpdir, "flow_evals.jsonl"),
                "--eval",
                eval_name,
                "--output",
                result_file,
            ],
            cwd=str(SCRIPT_DIR),
            check=False,
        )
        with open(result_file) as f:
            data = json.load(f)
        results = data.get("results", [])
        instance_id = f"facebook__flow-{eval_name}"
        matching = [r for r in results if r.get("instance_id") == instance_id]
        self.assertEqual(
            len(matching),
            1,
            f"Expected exactly one result for '{eval_name}', got {len(matching)}",
        )
        res = matching[0]
        failing = [t["name"] for t in res.get("tests", []) if not t["passed"]]
        self.assertEqual(
            failing,
            [],
            f"Failing graders for '{eval_name}': {', '.join(failing)}",
        )

    test_method.__name__ = f"test_{eval_name}"
    return test_method


for _name in _eval_names:
    setattr(EvalDryRunTests, f"test_{_name}", _make_test(_name))
