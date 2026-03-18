# check-test: Node.js Port of Flow's Bash Test Runner

## Background

Flow's test suite consists of 920 test directories under `flow/tests/`. Each test
has a `.flowconfig`, a `.exp` expected-output file, and optionally a `.testconfig`
and a test script (`test.sh` or `test.js`). Historically these tests were
orchestrated by a set of bash scripts:

| Bash script | Lines | Role |
|---|---|---|
| `runtests.sh` | 422 | CLI entry point, test discovery, parallel dispatch via `xargs -P` |
| `scripts/run-one-test` | 632 | Per-test execution: config parsing, server management, diff comparison |
| `scripts/lib/runtests-common.sh` | 14 | Shared exit-code constants |

The `check-test` module replaces all three with 3,239 lines of Flow-typed
JavaScript, exposed as the `./tool check-test` subcommand. The port is
cross-platform (Linux/macOS/Windows) and runs tests as async Node.js promises
with a configurable-parallelism work queue, replacing `xargs -P`.

## Architecture

```
checkTestCommand.js           CLI entry point (flags, arg parsing)
        |
checkTestRunner.js             Test discovery, env setup, result reporting
        |
checkRunQueue.js               Parallel execution queue (Promise-based)
        |
checkRunOneTest.js             Per-test orchestration (the core)
       / \
      /   \
checkTestConfig.js           checkTestHelpers.js
(.testconfig parser)         (TestContext for .js tests)
      |                            |
checkAnnotateExports.js      checkExecFilePromise.js
(annotate-exports mode)      (child_process wrappers, splitShellArgs)
      |
checkDiffCompare.js
(output diffing, <VERSION> substitution)
```

## File-by-File Reference

### `checkTestCommand.js` (172 lines)

CLI subcommand definition using flow-dev-tools' `Base` command framework.
Parses flags that mirror `runtests.sh`'s options:

| Flag | Short | Bash equivalent |
|---|---|---|
| `--bin` | `-b` | positional `FLOW_BINARY` |
| `--tests-dir` | `-d` | `-d DIR` |
| `--filter` | `-f` | `-f TEST_FILTER` |
| `--test` | `-t` | `-t TEST` |
| `--check-only` | `-c` | `-c` |
| `--saved-state` | `-s` | `-s` |
| `--long-lived-workers` | `-L` | `-L` |
| `--record` | `-r` | `-r` |
| `--quiet` | `-q` | `-q` |
| `--verbose` | `-v` | `-v` |
| `--json` | `-j` | `-j` |
| `--list` | `-l` | `-l` |

Default parallelism is 16 (matching `runtests.sh`'s `$FLOW_RUNTESTS_PARALLELISM`
default).

### `checkTestRunner.js` (452 lines)

Top-level orchestration, replacing `runtests.sh`'s main body.

**Responsibilities:**
- Sets environment variables (`IN_FLOW_TEST`, `FLOW_LOG_LEVEL`, `FLOW_MAX_WORKERS`,
  `FLOW_GIT_BINARY`, `FLOW_NODE_BINARY`). Saves and restores originals to avoid
  permanent `process.env` mutation.
- Discovers test directories by reading `testsDir` and sorting alphabetically.
- Applies the filter regex to test names.
- Creates job objects and passes them to `CheckRunQueue`.
- Prints colored results with pass/fail/skip/error tallies.
- Supports `--json` mode (outputs `{test_name: boolean}` map).
- Supports `--record` mode (updates `.exp` files from actual output).
- Sets `process.exitCode = 1` on failure (not `process.exit()`, allowing stdout
  to flush when piped).

### `checkRunQueue.js` (217 lines)

Promise-based work queue with configurable parallelism.

Replaces `runtests.sh`'s `xargs -P $FLOW_RUNTESTS_PARALLELISM`. Starts up to
`parallelism` concurrent test promises, launching a new one each time one
completes. Uses `setImmediate` between jobs to avoid starving the event loop.

Displays a TTY status line (`[N/M] RUNNING: test_name`) that updates in place
using ANSI escape codes, cleared before final output.

Includes defensive error handling: synchronous errors, promise rejections, and
internal queue errors all settle the affected job as `RUNTEST_ERROR` and keep the
queue running. A `_resolveWithErrors` fallback ensures the queue promise never
hangs.

### `checkTestConfig.js` (139 lines)

Parses `.testconfig` files, replacing the awk-based parsing in `run-one-test`.

The `.testconfig` format is colon-delimited key-value pairs:

```
shell: test.sh
cmd: full-check
auto_start: true
stdin: input.txt
ignore_stderr: false
cwd: subdir
file_watcher: dfind
start_args: --lazy-mode none
wait_for_recheck: false
skip_saved_state: true
saved_state_only: true
git: true
skip_rust_port: true
```

**Behavioral match with bash:**
- Comments (`#`) and blank lines are skipped.
- `cmd` and `start_args` values use `.trim()` on the full value after the colon,
  matching bash's awk field-splitting which normalizes leading whitespace.
- Shell/cmd interaction: if `shell` is set but `cmd` is not explicitly set, `cmd`
  is cleared (matching bash's processing order where `shell` clears `cmd` unless
  `cmd` is re-specified).

### `checkRunOneTest.js` (978 lines)

The core per-test executor, replacing `scripts/run-one-test`.

**Flow of execution:**

1. **Pre-checks:** Verifies `.exp` file and `.flowconfig`/`.testconfig` exist.
   Checks for `rust_port` variant. Handles Windows symlink skips.

2. **Temp directory setup:** Creates a temp directory, copies the test directory
   into it, moves the `.exp` file out of the working copy.

3. **Config parsing:** Calls `parseTestConfig` on the temp copy.

4. **Flowconfig analysis:** Reads `.flowconfig` in the working directory to
   determine `noFlowlib` (whether to pass `--no-flowlib`) and to enforce the
   `all=(true|false)` requirement.

5. **Skip conditions:** Checks `skip_saved_state`, `saved_state_only`, `git`,
   `skip_rust_port`, and `check_only`.

6. **Environment setup:** Builds an env object with `FLOW_TEMP_DIR`, `IN_FLOW_TEST`,
   `FLOW_LOG_LEVEL`, `FLOW_LOG_FILE`, `FLOW_MONITOR_LOG_FILE`, `FLOW`, `VERSION`,
   and prepends `scripts/tests_bin` to `PATH`.

7. **Execution mode dispatch** (one of four):

   - **`full-check`**: Runs `flow full-check . [--no-flowlib] --strip-root
     --show-all-errors --long-lived-workers N`.

   - **`annotate-exports`**: Delegates to `runAnnotateExports()` which runs a
     three-phase codemod/autofix/compare pipeline.

   - **Shell `.sh`**: Builds an inline bash script containing all helper functions
     (`assert_ok`, `start_flow`, `create_saved_state`, `queries_in_file`, etc.)
     and `source`s the test script. This preserves exact bash compatibility for
     the 266 existing `.sh` test scripts.

   - **Shell `.js`**: Creates a `TestContext`, optionally auto-starts a Flow
     server, `require()`s the test script from the **original** test directory
     (so babel transforms and relative requires work), and calls the exported
     function. Clears the module cache after loading so subsequent tests get
     fresh modules.

8. **Output handling:** Writes stdout to `.out` file. Handles `ignore_stderr`:
   when true, stderr goes to `.err`; when false, stderr is appended to stdout.

9. **Diff comparison:** Calls `diffOutput` to compare `.out` with `.exp`.

10. **Artifact management:** On failure/error, copies `.out`, `.log`,
    `.monitor_log`, `.err`, `.diff` back to the source test directory for
    debugging. On success, cleans up any leftover artifacts from previous runs.

11. **Cleanup:** The `finally` block stops any running Flow server (best-effort,
    with 30s timeout) and removes the temp directory.

### `checkTestHelpers.js` (646 lines)

The `TestContext` class, providing the API for `.js` test scripts. This is the
Node.js equivalent of the helper functions defined at the top of
`scripts/run-one-test` that `.sh` test scripts use via `source`.

**Key APIs:**

| Method | Bash equivalent | Description |
|---|---|---|
| `flowCmd(args)` | `$FLOW args` | Run a flow command, return `[code, stdout, stderr]` |
| `assertOk(args)` | `assert_ok $FLOW args` | Run flow command, assert exit 0, print stdout |
| `assertErrors(args)` | `assert_errors $FLOW args` | Assert exit code 2 |
| `assertOne(args)` | `assert_one $FLOW args` | Assert exit code 1 |
| `assertExit(code, args)` | `assert_exit code $FLOW args` | Assert specific exit code |
| `startFlow(root, ...opts)` | `start_flow root opts` | Start server (assert exit 0) |
| `startFlowUnsafe(root, ...opts)` | `start_flow_unsafe root opts` | Start server (return exit code) |
| `createSavedState(root, name)` | `create_saved_state root name` | Create saved state files |
| `queryAtPos(query, file, line, col, ...flags)` | `query_at_pos` | Run a positional query |
| `queriesInFile(query, file, ...flags)` | `queries_in_file` | Find `^` markers and run queries |
| `grepLines(text, pattern)` | `grep pattern` | BRE-compatible line filtering |
| `sedReplace(text, search, replace)` | `sed 's/search/replace/g'` | BRE-compatible substitution |
| `remove(...files)` | `rm + force-recheck` | Delete files and force-recheck |
| `move(src, dst)` | `mv + force-recheck` | Move file and force-recheck |
| `copy(src, dst)` | `cp + force-recheck` | Copy file and force-recheck |
| `print(text)` / `printf(fmt, ...)` | `echo` / `printf` | Append to output buffer |
| `showSkippingStats(logFile)` | `show_skipping_stats` | Extract skipping stats from log |
| `sleep(ms)` | `sleep N` | Async delay |

**BRE-to-JS regex conversion** (`_convertBREtoJS`):

Bash `grep` and `sed` use Basic Regular Expressions where `+`, `?`, `|`, `{`,
`}`, `(`, `)` are literal by default and require a backslash to become
metacharacters (the opposite of JS regex). The converter handles:

- `\+` `\?` `\|` `\{` `\}` `\(` `\)` → removes backslash (JS metacharacter)
- Bare `+` `?` `|` `{` `}` `(` `)` → adds backslash (JS literal)
- `\d` `\w` `\b` `\s` (and uppercase variants) → literal character (BRE has
  no shorthand classes; in JS these are special)
- `\\` → preserved as literal backslash

**Known limitation:** GNU grep's `\<` and `\>` word boundary markers are not
converted. They pass through as literal `<` and `>`, which won't match. No
existing tests use these markers.

### `checkAnnotateExports.js` (362 lines)

Handles the `annotate-exports` test mode, a three-phase pipeline:

1. **Codemod phase:** Finds all `.js`/`.js.flow` files, runs
   `flow codemod annotate-exports`, records changes.
2. **Autofix phase:** Restores originals, starts a Flow server, runs
   `flow autofix exports` on each file, stops the server.
3. **Comparison phase:** Diffs codemod output against autofix output using
   `normalDiff` (normal diff format).

**`normalDiff` function:** Reimplements GNU `diff` normal format output (the
format without `-u` flag) using the `diff` npm package's `diffLines` function.
Produces `N,NcN,N` / `N,NdN` / `NaN,N` range headers with `< ` / `> ` /
`---` markers. Handles `\ No newline at end of file` annotations. Verified
against GNU `diff` for correctness across 15 edge cases.

**`findFiles` function:** Async recursive file discovery with concurrency
limiting (32 concurrent `fs.stat` calls) to avoid exhausting file descriptors
in the parallel test runner.

### `checkExecFilePromise.js` (187 lines)

Child process execution wrappers.

**`execFilePromise(cmd, args, options, stdinData)`:** Wraps `child_process.execFile`
in a Promise. Always resolves (never rejects) — non-zero exit codes are returned
in the result object. Default timeout: 10 minutes. Default max buffer: 100 MB.
Handles stdin piping with EPIPE error suppression.

**`execShellPromise(cmdString, options, stdinData)`:** Same as above but uses
`child_process.exec` (shell execution). Used for `.testconfig` `cmd` values
containing shell metacharacters (`|`, `&`, `;`, `<`, `>`, `(`, `)`, `$`, `` ` ``).

**`splitShellArgs(argString)`:** Splits a shell argument string into an array,
handling single quotes, double quotes, and backslash escapes. Matches bash
quoting semantics including:
- Single quotes: no escaping, everything is literal
- Double quotes: only `$`, `` ` ``, `"`, `\`, `\n` are backslash-escapable;
  all other `\X` sequences preserve the backslash
- Preserves empty quoted arguments (`''` or `""`)

**`existsAsync(path)`:** Async file existence check using `fs.promises.access`.

### `checkDiffCompare.js` (86 lines)

Output comparison and recording.

**`diffOutput(expFile, outFile, version)`:** Reads the `.exp` and `.out` files,
normalizes line endings (`\r\n` and `\r` → `\n`), substitutes `<VERSION>`
placeholders in the `.exp` file with the actual Flow version, and generates a
unified diff using `createTwoFilesPatch` from the `diff` npm package.

**`recordOutput(outFile, expFile, version)`:** For `--record` mode. Reads actual
output, replaces the literal version string with `<VERSION>`, and writes the
result to the `.exp` file.

**`substituteVersion` / `substituteVersionForRecord`:** Handle bidirectional
`<VERSION>` ↔ version-string replacement. `substituteVersionForRecord` properly
escapes regex-special characters in semver strings (e.g., `+` in build metadata).

## Execution Modes

The test runner supports four execution modes, determined by `.testconfig`:

### 1. Full-check mode (default)

When `cmd: full-check` (the default) and no `shell` is set. Runs:

```
flow full-check . [--no-flowlib] --strip-root --show-all-errors \
  --long-lived-workers N
```

No server is started; it's a single-shot invocation.

### 2. Annotate-exports mode

When `cmd: annotate-exports [extra-args]`. Runs the three-phase
codemod/autofix/compare pipeline described in `checkAnnotateExports.js`.

### 3. Shell mode (`.sh`)

When `shell: test.sh`. An inline bash script is constructed containing all
helper functions (`assert_ok`, `start_flow`, `create_saved_state`,
`queries_in_file`, etc.) and the test script is `source`d within it. The
server is auto-started if `auto_start: true` (default). This mode maintains
exact bash compatibility for existing `.sh` tests.

### 4. Script mode (`.js`)

When `shell: test.js`. A `TestContext` is created, the server is optionally
auto-started, and the `.js` test module is `require()`d and called with the
context. The script is loaded from the **original** test directory (not the
temp copy) so that babel transforms from `bin/tool` apply and relative
`require()` paths resolve correctly.

## Key Design Decisions

### Temp directory isolation

Each test runs in a temp directory copy. This prevents tests from polluting each
other and matches `run-one-test`'s behavior. The `.exp` file is moved out of the
copy so it's not affected by test execution.

### Server lifecycle management

A `serverStarted` flag tracks whether a Flow server was started. The `finally`
block performs best-effort cleanup with a 30-second timeout. For `.sh` mode, the
inline bash script handles its own server stop, so the flag is cleared to avoid
redundant cleanup.

### Artifact management

On failure: `.out`, `.log`, `.monitor_log`, `.err`, `.diff` are copied back to
the source test directory for debugging.

On success: these artifacts are cleaned from the source directory (removing
leftovers from previous failing runs).

### Module cache management for `.js` tests

After `require()`ing a `.js` test script, the resolved path is immediately
captured and deleted from `require.cache`. This ensures that if the same test
runs again (e.g., in a different saved-state mode), it gets a fresh module
rather than a cached version.

## Behavioral Differences from Bash

The port is behaviorally faithful to the bash implementation. The following
known differences exist but do not affect test results:

| Area | Bash behavior | Node.js behavior | Impact |
|---|---|---|---|
| `.flowconfig` `all=` check | `grep -E "all=(true\|false)"` — substring match, matches commented-out lines | `^[ \t]*all=(true\|false)\b` with `m` flag — line-start anchored | Stricter; rejects commented-out `# all=true`. No existing test has this pattern. |
| BRE `\<` `\>` word boundaries | GNU grep matches word boundaries | Pass through as literal `<` `>` (no match) | No existing tests use these markers. |
| `ignore_stderr: false` in full-check | `2>` truncates `.out` file (bash bug — stdout can be corrupted) | Appends stderr after stdout | Improvement; no test relies on the truncation bug. |
| `.testconfig` whitespace | awk normalizes internal whitespace (`a  b` → `a b`) | `.trim()` preserves internal whitespace | Functionally equivalent after `splitShellArgs` splits on whitespace. |
| Signal-killed processes | Exit code `128 + signal_number` | Exit code `1` | Only affects processes killed by signals, which shouldn't happen in normal test execution. |
