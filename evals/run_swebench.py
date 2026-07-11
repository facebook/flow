#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""Run Flow evals from SWE-bench JSONL and grade results.

For each eval instance:
1. Sets up a temp dir with input files
2. Calls Claude (or applies gold patch in --dry-run mode)
3. Runs the grading script from test_patch
4. Reports TAP results

Usage:
  python3 run_swebench.py [options]
  make run                              # compile + run all
  make run ARGS="--eval error_001"      # single eval
  make run ARGS="--eval error_001 --eval error_002"  # multiple evals
  make run ARGS="--eval 'error_*'"     # glob pattern
  make dry-run                          # validate with gold patches
"""

import argparse
import concurrent.futures
import fnmatch
import json
import os
import re
import shutil
import signal
import subprocess
import sys
import tempfile
import threading
import time
from pathlib import Path


SCRIPT_DIR = Path(__file__).resolve().parent
DEFAULT_JSONL = SCRIPT_DIR / "build" / "swebench" / "flow_evals.jsonl"
# Default to the `flow` binary installed by `yarn install` (the flow-bin npm
# package). Pass --flow-bin to point at a locally built binary instead.
DEFAULT_FLOW_BIN = SCRIPT_DIR / "node_modules" / ".bin" / "flow"
DEFAULT_MODEL = "claude-sonnet-5"


# Track per-eval workdirs so a SIGTERM/SIGINT can stop their Flow servers
# instead of orphaning them.
#
# These registry guards are RLock, not Lock: _handle_shutdown_signal runs on the
# main thread and re-enters them. In serial mode (-j 1) run_eval also runs on the
# main thread, so a signal landing while it holds one of these would self-deadlock
# on a plain Lock — RLock lets the same thread re-acquire.
_active_workdirs = set()
_active_workdirs_lock = threading.RLock()
_signal_flow_bin = None

# Track live `claude` subprocesses the same way. They run in their own sessions
# (start_new_session=True), so a Ctrl-C on the runner's process group never
# reaches them — the signal handler has to tear them down explicitly or they
# keep running (and billing) after the runner exits.
_active_procs = set()
_active_proc_groups = {}
_active_procs_lock = threading.RLock()

# Serializes _terminate_process across its three callers (watchdog timer thread,
# run_claude's finally block, and the signal handler). Without it a second caller
# could see the process alive (poll → getpgid) while the first reaps it, then
# killpg a group whose leader PID has since been recycled. RLock for the same
# main-thread reentrancy reason as the registry guards above.
_terminate_lock = threading.RLock()


def _register_workdir(workdir):
    with _active_workdirs_lock:
        _active_workdirs.add(workdir)


def _unregister_workdir(workdir):
    with _active_workdirs_lock:
        _active_workdirs.discard(workdir)


def _register_proc(proc):
    try:
        pgid = os.getpgid(proc.pid)
    except ProcessLookupError:
        pgid = proc.pid
    with _active_procs_lock:
        _active_procs.add(proc)
        _active_proc_groups[proc] = pgid


def _unregister_proc(proc):
    with _active_procs_lock:
        _active_procs.discard(proc)
        _active_proc_groups.pop(proc, None)


def _handle_shutdown_signal(signum, frame):
    # Runs on the main thread, interrupting it mid-execution. It re-enters the
    # registry guards and _terminate_process, all of which use RLock so a signal
    # arriving while the main thread already holds one (serial mode) re-acquires
    # instead of self-deadlocking.
    # Kill the agent subprocesses first so they can't spawn more Flow checks,
    # then stop any Flow servers they left behind.
    with _active_procs_lock:
        procs = list(_active_procs)
    for proc in procs:
        _terminate_process(proc)
    if _signal_flow_bin is not None:
        with _active_workdirs_lock:
            wds = list(_active_workdirs)
        for wd in wds:
            stop_flow_server(wd, _signal_flow_bin)
    sys.exit(128 + signum)


def find_eval_dir(instance_id):
    """Map instance_id back to eval source directory."""
    # instance_id format: facebook__flow-{eval_name}
    eval_name = instance_id.replace("facebook__flow-", "", 1)
    evals_dir = SCRIPT_DIR / "evals"
    for category_dir in sorted(evals_dir.iterdir()):
        if not category_dir.is_dir() or not category_dir.name[0:2].isdigit():
            continue
        # Standard format: eval_name is a direct subdirectory
        candidate = category_dir / eval_name
        if candidate.is_dir():
            return candidate
        # Project-pattern format: find a project dir whose name is a prefix of
        # eval_name. Eval names are built as f"{project_name}_{stem}" from the
        # project's candidate_prompts/*.md stems, so confirm the remainder maps
        # to an actual prompt — otherwise a shorter project name that is a prefix
        # of a longer one (e.g. `foo` vs `foo_bar`) would silently shadow it.
        for subdir in sorted(category_dir.iterdir()):
            if not subdir.is_dir() or not (subdir / "context").is_dir():
                continue
            prefix = subdir.name + "_"
            if not eval_name.startswith(prefix):
                continue
            stem = eval_name[len(prefix) :]
            if (subdir / "candidate_prompts" / f"{stem}.md").is_file():
                return subdir
    return None


def setup_workdir(eval_dir, workdir, flow_bin):
    """Copy input/ files into the working directory and set up Flow binary."""
    input_dir = (
        eval_dir / "context" if (eval_dir / "context").is_dir() else eval_dir / "input"
    )
    for src in input_dir.rglob("*"):
        if src.is_file():
            rel = src.relative_to(input_dir)
            dst = workdir / rel
            dst.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(src, dst)

    # Copy shared flow-typed lib definitions if the eval's .flowconfig references them.
    # Project-pattern evals may use relative paths like ../../../../flow-typed/environment
    # that resolve locally but not in a flat tempdir workdir — rewrite them to flow-typed/...
    flowconfig = workdir / ".flowconfig"
    if flowconfig.is_file() and "flow-typed" in flowconfig.read_text():
        flow_typed_dir = SCRIPT_DIR / "flow-typed"
        if flow_typed_dir.is_dir():
            dst_flow_typed = workdir / "flow-typed"
            shutil.copytree(flow_typed_dir, dst_flow_typed, dirs_exist_ok=True)
        config_text = flowconfig.read_text()
        config_text = re.sub(r"\.\./+flow-typed", "flow-typed", config_text)
        flowconfig.write_text(config_text)

    # Create a wrapper script so `./flow` works in the temp dir
    flow_wrapper = workdir / "flow"
    flow_wrapper.write_text(f'#!/bin/bash\nexec {flow_bin.resolve()} "$@"\n')
    flow_wrapper.chmod(0o755)


def prewarm_flow_server(workdir, flow_bin):
    """Start the Flow server for workdir so the agent's first check is fast.

    Best-effort: at high parallelism the server may not be ready within the
    timeout — the agent will just face a cold server on its first check.
    Not called in dry-run mode since flow_check.sh uses `flow full-check`
    which is foreground-only and does not use the daemon.
    """
    try:
        subprocess.run(
            [str(flow_bin.resolve()), "check", str(workdir)],
            cwd=workdir,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            timeout=120,
            check=False,
        )
    except subprocess.TimeoutExpired:
        pass


def apply_patch(workdir, patch):
    """Apply a unified diff patch to the working directory. Returns (ok, error_msg)."""
    result = subprocess.run(
        ["patch", "-p1", "--no-backup-if-mismatch"],
        input=patch,
        text=True,
        cwd=workdir,
        capture_output=True,
    )
    if result.returncode != 0:
        return False, result.stderr.strip()
    return True, ""


def _registered_process_group(proc):
    with _active_procs_lock:
        return _active_proc_groups.get(proc)


def _process_group_exists(pgid):
    try:
        os.killpg(pgid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def _wait_for_process_exit(
    proc,
    group,
    timeout,
):
    if group is None:
        try:
            proc.wait(timeout=timeout)
            return True
        except subprocess.TimeoutExpired:
            return False

    deadline = time.monotonic() + timeout
    while True:
        try:
            proc.wait(timeout=0)
        except subprocess.TimeoutExpired:
            pass
        if not _process_group_exists(group):
            return True
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            return False
        time.sleep(min(0.1, remaining))


def _terminate_process(proc):
    # `claude` spawns a `native/claude` grandchild; terminating only the direct
    # child leaves that grandchild alive holding the stdout pipe open, which
    # wedges the reader in run_claude forever. The process is launched with
    # start_new_session=True so it leads its own process group — signal the whole
    # group so every descendant dies. Guard on the process being its own group
    # leader so we can never signal the runner's own group by accident.
    #
    # Hold _terminate_lock for the whole poll → getpgid → kill → wait sequence so
    # concurrent callers can't reap the child mid-sequence and leave us signalling
    # a recycled PID's group. Every wait is bounded: an unbounded wait would pin
    # the lock if the process can't be reaped (e.g. stuck in uninterruptible
    # sleep), and because _handle_shutdown_signal terminates procs on the main
    # thread, a wedged worker thread would then stall Ctrl-C shutdown for every
    # other proc (RLock only prevents *same*-thread self-deadlock).
    with _terminate_lock:
        registered_group = _registered_process_group(proc)
        try:
            pgid = os.getpgid(proc.pid)
        except ProcessLookupError:
            pgid = registered_group
        group = (
            pgid
            if pgid == proc.pid
            else registered_group
            if registered_group == proc.pid
            else None
        )
        if group is None and proc.poll() is not None:
            return

        def _kill(sig):
            try:
                if group is not None:
                    os.killpg(group, sig)
                else:
                    proc.send_signal(sig)
            except (ProcessLookupError, PermissionError):
                pass

        _kill(signal.SIGTERM)
        if _wait_for_process_exit(proc, group, timeout=5):
            return
        _kill(signal.SIGKILL)
        if not _wait_for_process_exit(proc, group, timeout=5):
            # Unreapable even after SIGKILL. Abandon it rather than block:
            # holding the lock here indefinitely would stall shutdown of every
            # other proc (see lock rationale above).
            print(
                f"warning: pid {proc.pid} did not exit after SIGKILL; "
                "abandoning it to avoid blocking shutdown",
                file=sys.stderr,
            )


def run_claude(
    workdir,
    problem_statement,
    model,
    trajectory_path=None,
    max_turns=None,
    timeout=300,
):
    """Call Claude CLI in non-interactive mode with optional turn limit.

    Returns a dict with keys: turns, cost_usd, time_s, exceeded_max_turns,
    and the raw trajectory messages.
    """
    # Auth comes from the standard Claude CLI configuration (the ANTHROPIC_API_KEY
    # environment variable, or an apiKeyHelper set in the user's own --settings).
    cmd = [
        "claude",
        "-p",
        problem_statement,
        "--bare",
        "--model",
        model,
        "--allowedTools",
        "Edit,Write,Read,Bash",
        "--output-format",
        "stream-json",
        "--verbose",
    ]

    proc = subprocess.Popen(
        cmd,
        cwd=workdir,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
        # Run in a new session so `claude` and its `native/claude` grandchild
        # share a process group the watchdog can kill as a unit (see
        # _terminate_process). This also detaches them from the runner's process
        # group, so a Ctrl-C no longer reaches them — the signal handler tears
        # them down via _active_procs instead.
        start_new_session=True,
    )
    _register_proc(proc)

    messages = []
    turns = 0
    cost_usd = 0.0
    num_turns_reported = 0
    exceeded = False
    t0 = time.time()

    # Watchdog: terminates the process after timeout even if stdout blocks
    timed_out = threading.Event()

    def _on_timeout():
        timed_out.set()
        _terminate_process(proc)

    timer = threading.Timer(timeout, _on_timeout)
    timer.start()

    try:
        for line in proc.stdout:
            line = line.strip()
            if not line:
                continue
            try:
                msg = json.loads(line)
            except json.JSONDecodeError:
                continue

            messages.append(msg)

            if msg.get("type") == "assistant":
                turns += 1
                if max_turns is not None and turns >= max_turns:
                    exceeded = True
                    break
            elif msg.get("type") == "result":
                num_turns_reported = msg.get("num_turns", 0)
                cost_usd = msg.get("total_cost_usd", 0.0)
    finally:
        timer.cancel()
        if timed_out.is_set():
            exceeded = True
        _terminate_process(proc)
        _unregister_proc(proc)

    elapsed = time.time() - t0

    if trajectory_path:
        trajectory_path.parent.mkdir(parents=True, exist_ok=True)
        trajectory_path.write_text("\n".join(json.dumps(m) for m in messages) + "\n")

    return {
        "turns": num_turns_reported or turns,
        "cost_usd": cost_usd,
        "time_s": round(elapsed, 1),
        "exceeded_max_turns": exceeded,
        "messages": messages,
    }


def stop_flow_server(workdir, flow_bin):
    """Stop any Flow server running in the workdir."""
    try:
        subprocess.run(
            [str(flow_bin.resolve()), "stop", str(workdir)],
            capture_output=True,
            timeout=10,
        )
    except Exception:
        pass


def environment_note(test_patch):
    """Build the environment instruction that matches how the eval is graded.

    The no_extra_flow_errors grader is what makes iterating on Flow errors
    costly, so the note tracks its threshold (read from the compiled grading
    script) rather than being one-size-fits-all:
      - threshold 0 (strict code-writing): ask for one-shot correctness, so a
        model that loops on errors is fairly penalized.
      - threshold 1 (error-fixing): the one initial check that reveals the bug
        is expected, so tell the model to check first, then fix.
      - grader absent (config/tooling): iterating on errors is legitimate, so
        just point at the type-checker.
    """
    base = "\n\nEnvironment: Edit files in the current directory. "
    if "FLOW_ERROR_THRESHOLD=0" in test_patch:
        return base + (
            "Write code that type-checks on the first attempt. Do not run "
            "`./flow check` to inspect the starting state or while you work — "
            "reason the types through yourself. You may run `./flow check .` a "
            "single time, only once you believe the code is complete, to confirm "
            "it is clean; you should not need to iterate on Flow errors."
        )
    if "FLOW_ERROR_THRESHOLD=1" in test_patch:
        return base + (
            "Run `./flow check .` to see the error, then fix it so the code "
            "type-checks cleanly."
        )
    return base + "Run `./flow check .` to type-check."


def run_eval(instance, args):
    """Run a single eval instance. Returns result dict."""
    instance_id = instance["instance_id"]
    eval_dir = find_eval_dir(instance_id)
    if eval_dir is None:
        return {"instance_id": instance_id, "error": "eval dir not found", "score": 0}

    workdir = Path(tempfile.mkdtemp(prefix=f"flow-eval-{instance_id}-"))
    _register_workdir(workdir)
    log = []

    try:
        # Setup
        setup_workdir(eval_dir, workdir, Path(args.flow_bin))
        if not args.dry_run:
            prewarm_flow_server(workdir, Path(args.flow_bin))

        exceeded = False
        trajectory_path = None

        if args.dry_run:
            # Apply gold patch
            ok, err = apply_patch(workdir, instance["patch"])
            if not ok:
                return {
                    "instance_id": instance_id,
                    "error": f"gold patch failed: {err}",
                    "score": 0,
                }
        else:
            # Call Claude — trajectory lives in the workdir (tmp), not the source tree
            trajectory_path = workdir / "trajectory.json"
            # Augment prompt with an environment note that matches how this eval
            # is graded (see environment_note).
            prompt = instance["problem_statement"] + environment_note(
                instance["test_patch"]
            )
            claude_result = run_claude(
                workdir,
                prompt,
                args.model,
                trajectory_path,
                max_turns=args.max_turns,
                timeout=args.timeout,
            )
            num_turns = claude_result["turns"]
            cost_usd = claude_result["cost_usd"]
            elapsed = claude_result["time_s"]

            exceeded = claude_result["exceeded_max_turns"]
            if exceeded:
                reason = (
                    f"max turns ({args.max_turns})"
                    if args.max_turns and num_turns >= args.max_turns
                    else f"timeout ({args.timeout}s)"
                )
                log.append(f"      STOPPED: {reason} — grading anyway")

            log.append(f"      {elapsed}s, {num_turns} turns, ${cost_usd:.4f}")
            log.append(f"      trajectory: {trajectory_path}")

        # Grade — write grading script to workdir
        tests_dir = workdir / "tests"
        tests_dir.mkdir(exist_ok=True)
        script_path = tests_dir / "run_graders.sh"
        script_path.write_text(instance["test_patch"])
        script_path.chmod(0o755)

        flow_bin = Path(args.flow_bin).resolve()
        env = os.environ.copy()
        env["FLOW_BIN"] = str(flow_bin)
        env["GRADERS_DIR"] = str(SCRIPT_DIR / "graders")
        eval_input_dir = (
            eval_dir / "context"
            if (eval_dir / "context").is_dir()
            else eval_dir / "input"
        )
        env["EVAL_INPUT_DIR"] = str(eval_input_dir.resolve())
        if trajectory_path is not None and trajectory_path.exists():
            env["TRAJECTORY_PATH"] = str(trajectory_path)

        try:
            result = subprocess.run(
                ["bash", str(script_path)],
                cwd=workdir,
                capture_output=True,
                text=True,
                env=env,
                timeout=120,
            )
        except subprocess.TimeoutExpired:
            return {
                "instance_id": instance_id,
                "error": "grading timeout after 120s",
                "score": 0,
                "turns": num_turns if not args.dry_run else None,
                "cost_usd": cost_usd if not args.dry_run else None,
                "time_s": elapsed if not args.dry_run else None,
                "log": log + ["      TIMEOUT: grading exceeded 120s"],
            }

        # Parse TAP
        tests = []
        passed = 0
        failed = 0
        for line in result.stdout.split("\n"):
            if line.startswith("ok "):
                m = re.match(r"ok \d+ (.+)", line)
                name = m.group(1) if m else line
                tests.append({"name": name, "passed": True})
                passed += 1
            elif line.startswith("not ok "):
                m = re.match(r"not ok \d+ (.+)", line)
                name = m.group(1) if m else line
                tests.append({"name": name, "passed": False})
                failed += 1

        total_graders = passed + failed
        score = passed / total_graders if total_graders > 0 else 0

        eval_result = {
            "instance_id": instance_id,
            "passed": passed,
            "failed": failed,
            "score": score,
            "tests": tests,
            "tap_output": result.stdout,
            "workdir": str(workdir) if (args.keep_tmp or not args.dry_run) else None,
            "trajectory": str(trajectory_path) if trajectory_path is not None else None,
            "log": log,
        }
        # Attach stats from Claude run (not available in dry-run)
        if not args.dry_run:
            eval_result["turns"] = num_turns
            eval_result["cost_usd"] = cost_usd
            eval_result["time_s"] = round(elapsed, 1)
            if exceeded:
                eval_result["exceeded_max_turns"] = True
        return eval_result

    finally:
        if not args.dry_run:
            stop_flow_server(workdir, Path(args.flow_bin))
        _unregister_workdir(workdir)
        if not args.keep_tmp and args.dry_run:
            shutil.rmtree(workdir, ignore_errors=True)


def main():
    parser = argparse.ArgumentParser(description="Run Flow evals and grade results")
    parser.add_argument(
        "--jsonl",
        default=str(DEFAULT_JSONL),
        help=f"Input JSONL file (default: {DEFAULT_JSONL.relative_to(SCRIPT_DIR)})",
    )
    parser.add_argument(
        "--model",
        default=DEFAULT_MODEL,
        help=f"Claude model (default: {DEFAULT_MODEL})",
    )
    parser.add_argument(
        "--eval",
        action="append",
        default=None,
        help="Only run evals matching this pattern (substring or glob). Can be repeated.",
    )
    parser.add_argument(
        "--category",
        action="append",
        default=None,
        help="Only run evals whose metadata.category matches (e.g. ts_to_flow). Can be repeated.",
    )
    parser.add_argument(
        "--tag",
        action="append",
        default=None,
        help="Only run evals carrying any of these metadata.tags. Can be repeated.",
    )
    parser.add_argument("--output", default=None, help="Results JSON output path")
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Apply gold patches instead of calling Claude",
    )
    parser.add_argument(
        "--flow-bin", default=str(DEFAULT_FLOW_BIN), help="Path to Flow binary"
    )
    parser.add_argument(
        "--keep-tmp",
        action="store_true",
        help="Keep temp directories in --dry-run mode (non-dry-run runs always "
        "preserve workdirs for inspection)",
    )
    parser.add_argument(
        "--max-turns",
        type=int,
        default=None,
        help="Max LLM turns before stopping the eval (default: no limit)",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=300,
        help="Per-eval wall-clock budget in seconds before the agent is stopped "
        "(default: 300). Raise it when the host filesystem is slow enough that "
        "agent exploration eats the budget, so an eval still yields a real result.",
    )
    parser.add_argument(
        "-l",
        "--limit",
        type=int,
        default=None,
        help="Limit the number of evals to run",
    )
    parser.add_argument(
        "--skip", type=int, default=0, help="Number of evals to skip (default: 0)"
    )
    parser.add_argument(
        "--list-inputs",
        action="store_true",
        help="List matching eval names without running them",
    )
    parser.add_argument(
        "--rerun-failed",
        action="store_true",
        help="Only run evals that failed in the previous results.json",
    )
    parser.add_argument(
        "-j",
        "--parallel",
        type=int,
        default=1,
        help="Number of evals to run in parallel (default: 1)",
    )
    args = parser.parse_args()

    global _signal_flow_bin
    _signal_flow_bin = Path(args.flow_bin)
    signal.signal(signal.SIGTERM, _handle_shutdown_signal)
    signal.signal(signal.SIGINT, _handle_shutdown_signal)

    if not os.path.isfile(args.jsonl):
        print(
            f"Error: {args.jsonl} not found. Run 'make compile' first.", file=sys.stderr
        )
        sys.exit(1)

    if not os.path.isfile(args.flow_bin):
        print(f"Error: Flow binary not found at {args.flow_bin}", file=sys.stderr)
        sys.exit(1)

    # Load instances
    instances = []

    def matches_eval_filters(instance_id, patterns):
        name = instance_id.replace("facebook__flow-", "")
        return any(
            fnmatch.fnmatch(name, p) if any(c in p for c in "*?[]") else p in name
            for p in patterns
        )

    failed_ids = None
    if args.rerun_failed:
        results_path = args.output or str(Path(args.jsonl).parent / "results.json")
        if os.path.isfile(results_path):
            with open(results_path) as f:
                prev = json.load(f)
            failed_ids = {
                r["instance_id"]
                for r in prev.get("results", [])
                if r.get("failed", 0) > 0 or "error" in r
            }
        else:
            print("No previous results found, running all evals.")

    with open(args.jsonl) as f:
        for line in f:
            inst = json.loads(line)
            if args.eval and not matches_eval_filters(inst["instance_id"], args.eval):
                continue
            if args.category and inst.get("category") not in args.category:
                continue
            if args.tag and not (set(args.tag) & set(inst.get("tags", []))):
                continue
            if failed_ids is not None and inst["instance_id"] not in failed_ids:
                continue
            instances.append(inst)

    if not instances:
        print("No matching evals found.", file=sys.stderr)
        sys.exit(1)

    # Apply skip/limit
    if args.skip:
        instances = instances[args.skip :]
    if args.limit is not None:
        instances = instances[: args.limit]

    if args.list_inputs:
        for inst in instances:
            print(inst["instance_id"].replace("facebook__flow-", ""))
        sys.exit(0)

    mode = "DRY RUN (gold patches)" if args.dry_run else f"model={args.model}"
    print(f"Running {len(instances)} eval(s) — {mode}\n")

    def print_result(result):
        iid = result["instance_id"].replace("facebook__flow-", "")
        if "error" in result:
            print(f"  \u2717 {iid:<45} ERROR: {result['error']}")
        else:
            total_graders = result["passed"] + result["failed"]
            graders = f"({result['passed']}/{total_graders})"
            if result["failed"] == 0:
                print(f"  \u2713 {iid:<45} {graders}")
            else:
                print(f"  \u2717 {iid:<45} {graders}")
                for t in result.get("tests", []):
                    if not t["passed"]:
                        print(f"      FAIL {t['name']}")
        for line in result.get("log", []):
            print(line)
        if result.get("workdir"):
            print(f"      workdir: {result['workdir']}")

    results = []
    total_passed = 0
    total_failed = 0

    if args.parallel > 1:
        with concurrent.futures.ThreadPoolExecutor(
            max_workers=args.parallel
        ) as executor:
            futures = {
                executor.submit(run_eval, inst, args): idx
                for idx, inst in enumerate(instances)
            }
            done_count = 0
            result_by_idx = {}
            for future in concurrent.futures.as_completed(futures):
                idx = futures[future]
                try:
                    result_by_idx[idx] = future.result()
                except Exception as e:
                    instance_id = instances[idx].get("instance_id", f"unknown-{idx}")
                    result_by_idx[idx] = {
                        "instance_id": instance_id,
                        "error": str(e),
                        "score": 0,
                    }
                done_count += 1
                name = result_by_idx[idx]["instance_id"].replace("facebook__flow-", "")
                bar_w = 20
                filled = int(bar_w * done_count / len(instances))
                bar = "█" * filled + "░" * (bar_w - filled)
                cols = shutil.get_terminal_size().columns
                msg = f"\r  {bar} {done_count}/{len(instances)} (latest: {name})"
                print(f"{msg:<{cols}}", end="", flush=True)
            print()  # finish the progress line
            results = [result_by_idx[i] for i in range(len(instances))]
            print()
            for result in results:
                print_result(result)
                if "error" not in result:
                    if result["failed"] == 0:
                        total_passed += 1
                    else:
                        total_failed += 1
    else:
        for _i, inst in enumerate(instances, 1):
            result = run_eval(inst, args)
            results.append(result)
            print_result(result)
            if "error" not in result:
                if result["failed"] == 0:
                    total_passed += 1
                else:
                    total_failed += 1

    # Summary table
    total = total_passed + total_failed
    print(f"\n{'=' * 80}")

    has_stats = any(r.get("turns") for r in results)
    if has_stats:
        hdr = f"{'Eval':<40} {'Result':<8} {'Graders':<10} {'Turns':<7} {'Time':<8} {'Cost':<8}"
        print(hdr)
        print("-" * 80)
        total_cost = 0.0
        total_time = 0.0
        for r in results:
            name = r["instance_id"].replace("facebook__flow-", "")
            if len(name) > 38:
                name = name[:35] + "..."
            if "error" in r:
                print(f"{name:<40} {'ERROR':<8} {'-':<10} {'-':<7} {'-':<8} {'-':<8}")
                continue
            status = "PASS" if r["failed"] == 0 else "FAIL"
            graders = f"{r['passed']}/{r['passed'] + r['failed']}"
            turns = str(r.get("turns", "-"))
            if r.get("exceeded_max_turns"):
                turns += " CUT"
            time_s = f"{r.get('time_s', 0):.0f}s" if r.get("time_s") else "-"
            cost = f"${r.get('cost_usd', 0):.4f}" if r.get("cost_usd") else "-"
            total_cost += r.get("cost_usd", 0)
            total_time += r.get("time_s", 0)
            print(
                f"{name:<40} {status:<8} {graders:<10} {turns:<7} {time_s:<8} {cost:<8}"
            )
        print("-" * 80)
        pct = f"{total_passed / total * 100:.0f}%" if total else "0%"
        print(
            f"{'Total':<40} {total_passed}/{total} ({pct})"
            f"{'':>10} {'':>7} {total_time:.0f}s{'':>4} ${total_cost:.4f}"
        )
    else:
        print(
            f"Results: {total_passed}/{total} passed ({total_passed / total * 100:.0f}%)"
            if total
            else "No results"
        )

    # Write results
    output_path = args.output or str(Path(args.jsonl).parent / "results.json")
    output_dir = os.path.dirname(output_path)
    if output_dir:
        os.makedirs(output_dir, exist_ok=True)
    with open(output_path, "w") as f:
        json.dump(
            {
                "model": args.model if not args.dry_run else "dry-run",
                "total": total,
                "passed": total_passed,
                "failed": total_failed,
                "score": total_passed / total if total else 0,
                "results": results,
            },
            f,
            indent=2,
        )
        f.write("\n")
    print(f"Results written to {output_path}")
    if not args.dry_run:
        trajectories = [
            Path(r["trajectory"])
            for r in results
            if r.get("trajectory") and Path(r["trajectory"]).exists()
        ]
        if len(trajectories) == 1:
            print(f"Trajectory (stream-json): {trajectories[0]}")
        elif trajectories:
            print("Trajectories (stream-json):")
            for t in trajectories:
                print(f"  {t}")
        workdirs = [
            (r["instance_id"].replace("facebook__flow-", ""), r["workdir"])
            for r in results
            if r.get("workdir")
        ]
        if workdirs:
            print(f"\nWorkdirs:")
            for name, wd in workdirs:
                print(f"  {name}: {wd}")


if __name__ == "__main__":
    main()
