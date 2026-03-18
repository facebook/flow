/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const fs = require('fs');
const {join, basename, resolve, delimiter} = require('path');
const os = require('os');

const {parseTestConfig} = require('./checkTestConfig');
const {diffOutput, recordOutput} = require('./checkDiffCompare');
const {TestContext} = require('./checkTestHelpers');
const {runAnnotateExports} = require('./checkAnnotateExports');
const {
  execFilePromise,
  execShellPromise,
  existsAsync,
  splitShellArgs,
} = require('./checkExecFilePromise');

// Return codes matching runtests-common.sh
const RUNTEST_SUCCESS = 0;
const RUNTEST_FAILURE = 1;
const RUNTEST_SKIP = 2;
const RUNTEST_MISSING_FILES = 3;
const RUNTEST_ERROR = 4;
const RUNTEST_MISSING_ALL_OPTION = 5;

export type TestResult = {
  status: number,
  name: string,
  diff?: string,
};

// Recursively copy directory contents (preserving symlinks).
// Limits concurrency to avoid exhausting file descriptors when
// copying test directories with many files in a parallel runner.
const COPY_CONCURRENCY = 32;

async function copyDirAsync(src: string, dst: string): Promise<void> {
  await fs.promises.mkdir(dst, {recursive: true});
  const entries = await fs.promises.readdir(src);
  for (let i = 0; i < entries.length; i += COPY_CONCURRENCY) {
    const batch = entries.slice(i, i + COPY_CONCURRENCY);
    await Promise.all(
      batch.map(async entry => {
        const srcPath = join(src, entry);
        const dstPath = join(dst, entry);
        const lstat = await fs.promises.lstat(srcPath);
        if (lstat.isSymbolicLink()) {
          const linkTarget = await fs.promises.readlink(srcPath);
          await fs.promises.symlink(linkTarget, dstPath);
        } else if (lstat.isDirectory()) {
          await copyDirAsync(srcPath, dstPath);
        } else {
          await fs.promises.copyFile(srcPath, dstPath);
        }
      }),
    );
  }
}

async function runOneTest(opts: {
  testDir: string,
  flowBin: string,
  version: string,
  checkOnly: boolean,
  savedState: boolean,
  longLivedWorkers: boolean,
  record: boolean,
}): Promise<TestResult> {
  const {testDir, flowBin, version, checkOnly, savedState, record} = opts;
  const longLivedWorkers = opts.longLivedWorkers ? '1' : '0';

  const name = basename(testDir);
  let expFileName = name + '.exp';

  // Check for rust_port variant
  if (
    (process.env.FLOW_RUST_PORT || '0') === '1' &&
    (await existsAsync(join(testDir, name + '.exp.rust_port')))
  ) {
    expFileName = name + '.exp.rust_port';
  }

  // Windows symlink skip
  if (
    process.platform === 'win32' &&
    (name === 'symlink' || name === 'node_tests')
  ) {
    return {status: RUNTEST_SKIP, name};
  }

  // Check required files — .exp file is always required.
  // For .flowconfig, we require it OR .testconfig (since .testconfig may
  // specify a cwd: where the .flowconfig lives, e.g. in a subdirectory).
  const hasExpFile = await existsAsync(join(testDir, expFileName));
  const hasFlowconfig = await existsAsync(join(testDir, '.flowconfig'));
  const hasTestconfig = await existsAsync(join(testDir, '.testconfig'));
  if (!hasExpFile || (!hasFlowconfig && !hasTestconfig)) {
    if (name === 'auxiliary' || name === 'callable') {
      return {status: RUNTEST_SKIP, name};
    }
    return {status: RUNTEST_MISSING_FILES, name};
  }

  // Create temp directory
  const tmpParent = await fs.promises.mkdtemp(
    join(os.tmpdir(), 'flow_check_test_'),
  );
  const tmpDir = join(tmpParent, name);
  let workDir = tmpDir; // hoisted so `finally` can stop the flow server
  let serverStarted = false; // track whether we started a flow server
  let env: {[string]: string | void} = {};

  try {
    await fs.promises.mkdir(tmpDir, {recursive: true});

    // Copy test directory to temp
    await copyDirAsync(testDir, tmpDir);

    // Copy fs.sh equivalent (parent's fs.sh) - not needed for JS but maintain structure
    const fsShPath = join(testDir, '..', 'fs.sh');
    if (await existsAsync(fsShPath)) {
      await fs.promises.copyFile(fsShPath, join(tmpParent, 'fs.sh'));
    }

    // Move exp file out of test dir (same filesystem, so rename is safe)
    const expSrc = join(tmpDir, expFileName);
    const expDst = join(tmpParent, expFileName);
    if (await existsAsync(expSrc)) {
      await fs.promises.rename(expSrc, expDst);
    }

    const outFile = join(tmpParent, name + '.out');
    const logFile = join(tmpParent, name + '.log');
    const monitorLogFile = join(tmpParent, name + '.monitor_log');
    const errFile = join(tmpParent, name + '.err');
    const diffFile = join(tmpParent, name + '.diff');

    // Parse config
    const config = await parseTestConfig(tmpDir);

    // Handle cwd - determine working directory
    workDir = config.cwd ? join(tmpDir, config.cwd) : tmpDir;

    // Determine flowlib - check .flowconfig in the working directory (after cwd applied)
    // Matches bash behavior: the check happens after `pushd "$cwd"`
    let noFlowlib = true;
    const flowconfigPath = join(workDir, '.flowconfig');
    if (await existsAsync(flowconfigPath)) {
      const flowconfigContent = await fs.promises.readFile(
        flowconfigPath,
        'utf8',
      );
      if (/no_flowlib/.test(flowconfigContent)) {
        noFlowlib = false;
      }

      // Check for all=(true|false) requirement
      if (!/^[ \t]*all=(true|false)\b/m.test(flowconfigContent)) {
        return {status: RUNTEST_MISSING_ALL_OPTION, name};
      }
    }

    // Skip conditions
    if (savedState && config.skip_saved_state) {
      return {status: RUNTEST_SKIP, name};
    }
    if (!savedState && config.saved_state_only) {
      return {status: RUNTEST_SKIP, name};
    }
    if (!process.env.FLOW_GIT_BINARY && config.git) {
      return {status: RUNTEST_SKIP, name};
    }
    if ((process.env.FLOW_RUST_PORT || '0') === '1' && config.skip_rust_port) {
      return {status: RUNTEST_SKIP, name};
    }
    if (checkOnly && config.cmd.trim() !== 'full-check') {
      return {status: RUNTEST_SKIP, name};
    }

    // Set up environment
    // Include tests_bin in PATH so the flow server can find helper scripts
    // (e.g. fetch_saved_state.sh for --saved-state-fetcher local).
    // Matches bash's `PATH="$THIS_DIR/scripts/tests_bin:$PATH"`.
    const testsBinDir = join(
      resolve(__dirname, '../../../../scripts'),
      'tests_bin',
    );
    env = {
      ...process.env,
      FLOW_TEMP_DIR: tmpParent,
      IN_FLOW_TEST: '1',
      FLOW_LOG_LEVEL: 'debug',
      FLOW_LOG_FILE: logFile,
      FLOW_MONITOR_LOG_FILE: monitorLogFile,
      FLOW: flowBin,
      VERSION: version,
      PATH: testsBinDir + delimiter + (process.env.PATH || ''),
    };

    let returnStatus = RUNTEST_SUCCESS;
    let outContent = '';
    let errContent = '';

    // Execute test based on mode
    if (config.cmd.trim() === 'full-check') {
      // Full-check mode
      if (savedState) {
        return {status: RUNTEST_SKIP, name};
      }

      const args = ['full-check', '.'];
      if (noFlowlib) {
        args.push('--no-flowlib');
      }
      args.push(
        '--strip-root',
        '--show-all-errors',
        '--long-lived-workers',
        longLivedWorkers,
      );

      const result = await execFilePromise(flowBin, args, {
        cwd: workDir,
        env,
      });
      outContent = result.stdout;
      if (config.ignore_stderr) {
        errContent = result.stderr;
      } else {
        outContent += result.stderr;
      }

      if (config.ignore_stderr && result.code !== 0 && result.code !== 2) {
        errContent += 'flow full-check return code: ' + result.code + '\n';
        returnStatus = RUNTEST_ERROR;
      }
    } else if (splitShellArgs(config.cmd.trim())[0] === 'annotate-exports') {
      // Annotate-exports mode
      // Mark serverStarted because runAnnotateExports starts a flow server
      // internally. If it throws before stopping the server, the finally
      // block will clean it up.
      serverStarted = true;
      const cmdArgs = config.cmd.trim().replace(/^annotate-exports\s*/, '');
      const annotResult = await runAnnotateExports({
        flowBin,
        testDir: workDir,
        noFlowlib,
        cmdArgs,
        logFile,
        monitorLogFile,
        waitForRecheck: config.wait_for_recheck,
        fileWatcher: config.file_watcher,
        longLivedWorkers,
        env,
      });
      // runAnnotateExports stops the server internally, so clear the flag
      // to avoid a redundant stop in the finally block.
      serverStarted = false;
      outContent = annotResult.output;
      errContent = annotResult.stderr;
      // Match bash behavior: only set RUNTEST_ERROR when ignore_stderr
      // is true. When ignore_stderr is false, the error exit code is
      // not treated as fatal — the test is judged by its output diff.
      if (config.ignore_stderr && annotResult.errorCode !== 0) {
        errContent +=
          'flow codemod return code: ' + annotResult.errorCode + '\n';
        returnStatus = RUNTEST_ERROR;
      }
    } else if (config.shell !== '') {
      // Shell/script mode
      if (config.shell.endsWith('.js')) {
        // Run as Node.js test script — use a single TestContext for both
        // auto-start and test execution so they share output state.
        const ctx = new TestContext({
          flowBin,
          testDir: workDir,
          logFile,
          monitorLogFile,
          noFlowlib,
          waitForRecheck: config.wait_for_recheck,
          fileWatcher: config.file_watcher,
          longLivedWorkers,
          savedState,
          tempDir: tmpParent,
        });

        if (config.auto_start) {
          const startArgs = config.start_args
            ? splitShellArgs(config.start_args)
            : [];

          if (savedState) {
            // Handle saved state: create saved state, then start with
            // --saved-state-fetcher (mirroring bash start_flow_unsafe
            // and the general cmd mode's saved state path).
            let flowconfigName = '.flowconfig';
            for (let i = 0; i < startArgs.length; i++) {
              if (
                startArgs[i] === '--flowconfig-name' &&
                i + 1 < startArgs.length
              ) {
                flowconfigName = startArgs[i + 1];
              }
            }

            const ok = await ctx.createSavedState(workDir, flowconfigName);
            if (!ok) {
              outContent = 'Failed to generate saved state\n';
              returnStatus = RUNTEST_ERROR;
            } else {
              const startResult = await execFilePromise(
                flowBin,
                [
                  'start',
                  '.',
                  ...(noFlowlib ? ['--no-flowlib'] : []),
                  '--wait',
                  '--wait-for-recheck',
                  config.wait_for_recheck,
                  '--saved-state-fetcher',
                  'local',
                  '--saved-state-no-fallback',
                  '--file-watcher',
                  config.file_watcher,
                  '--log-file',
                  logFile,
                  '--monitor-log-file',
                  monitorLogFile,
                  '--long-lived-workers',
                  longLivedWorkers,
                  ...startArgs,
                ],
                {cwd: workDir, env},
              );
              errContent += startResult.stderr;
              if (startResult.code !== 0) {
                outContent =
                  'flow start exited code ' + startResult.code + '\n';
                returnStatus = RUNTEST_ERROR;
              } else {
                serverStarted = true;
              }
            }
          } else {
            // Start server directly (suppressing stdout, capturing stderr
            // to errFile — matches bash's `> /dev/null 2>> "$abs_err_file"`)
            const startResult = await execFilePromise(
              flowBin,
              [
                'start',
                '.',
                ...(noFlowlib ? ['--no-flowlib'] : []),
                '--wait',
                '--wait-for-recheck',
                config.wait_for_recheck,
                '--file-watcher',
                config.file_watcher,
                '--log-file',
                logFile,
                '--monitor-log-file',
                monitorLogFile,
                '--long-lived-workers',
                longLivedWorkers,
                ...startArgs,
              ],
              {cwd: workDir, env},
            );
            errContent += startResult.stderr;
            if (startResult.code !== 0) {
              outContent = 'flow start exited code ' + startResult.code + '\n';
              returnStatus = RUNTEST_ERROR;
            } else {
              serverStarted = true;
            }
          }
        }

        if (returnStatus !== RUNTEST_ERROR) {
          try {
            // Require the test script from the ORIGINAL test directory
            // (not the temp copy) so that:
            // 1. The babel transform in bin/tool applies (it only covers
            //    files under testsDir, not temp dirs).
            // 2. Relative require()s like '../test_helpers' resolve
            //    correctly against the original tests/ tree.
            const originalScriptPath = join(
              testDir,
              config.cwd || '',
              config.shell,
            );
            // $FlowFixMe[unsupported-syntax] - dynamic require for test script loading
            const testModule = require(originalScriptPath);
            // Clear the module cache so that subsequent tests with the same
            // script name get a fresh module.
            // Capture the resolved path immediately after require() succeeds,
            // before the test runs (which might modify or delete files).
            const resolvedPath = require.resolve(originalScriptPath);
            delete require.cache[resolvedPath];
            const testFn =
              typeof testModule === 'function'
                ? testModule
                : testModule.default || testModule;
            if (typeof testFn !== 'function') {
              throw new Error(
                config.shell +
                  ' does not export a function (got ' +
                  typeof testFn +
                  ')',
              );
            }
            await testFn(ctx);
            outContent = ctx.getOutput();
          } catch (e) {
            outContent = ctx.getOutput();
            outContent += config.shell + ' exited code 1\n';
            if (e.stack) {
              errContent += e.stack + '\n';
            } else if (e.message) {
              errContent += e.message + '\n';
            }
            returnStatus = RUNTEST_ERROR;
          }

          // Stop server — only attempt if one was started, to avoid
          // spawning a useless process when auto_start is false.
          // Only clear the flag if stop succeeds so the finally block
          // can retry if the server is unresponsive.
          if (serverStarted) {
            const stopResult = await execFilePromise(flowBin, ['stop', '.'], {
              cwd: workDir,
              env,
            });
            if (stopResult.code === 0) {
              serverStarted = false;
            }
          }
        }
      } else {
        // Run as bash script (original test.sh)
        const scriptPath = join(workDir, config.shell);
        const noFlowlibEnv = noFlowlib ? '1' : '';
        const thisDir = resolve(__dirname, '../../../../scripts');
        // Construct the flowlib bash variable from the boolean
        const flowlibVal = noFlowlib ? ' --no-flowlib' : '';

        const bashEnv: {[string]: string | void} = {
          ...env,
          NO_FLOWLIB: noFlowlibEnv || undefined,
        };

        // Pass all dynamic values via environment variables to avoid
        // shell injection when paths contain spaces or special characters.
        const scriptEnv: {[string]: string | void} = {
          ...bashEnv,
          _CT_FLOW_BIN: flowBin,
          _CT_LOG_FILE: logFile,
          _CT_MONITOR_LOG_FILE: monitorLogFile,
          _CT_TEMP_DIR: tmpParent,
          _CT_SAVED_STATE: savedState ? '1' : '0',
          _CT_FLOWLIB: flowlibVal,
          _CT_WAIT_FOR_RECHECK: config.wait_for_recheck,
          _CT_FILE_WATCHER: config.file_watcher,
          _CT_LONG_LIVED_WORKERS: longLivedWorkers,
          _CT_THIS_DIR: thisDir,
          _CT_ERR_FILE: errFile,
          _CT_START_ARGS: config.start_args,
          _CT_AUTO_START: config.auto_start ? '1' : '0',
          _CT_SCRIPT_PATH: scriptPath,
        };

        // Build a bash command that includes all helper functions and variables
        // matching what run-one-test provides.
        // All dynamic values are read from environment variables set above
        // to avoid shell injection issues with paths containing spaces.
        const bashCmd = [
          'bash',
          '-c',
          `set -e
export FLOW="$_CT_FLOW_BIN"
export EXIT_OK=0
export EXIT_ONE=1
export EXIT_ERRS=2
export EXIT_INVALID_FLOWCONFIG=8
export EXIT_SERVER_ALREADY_RUNNING=11
export EXIT_COULD_NOT_FIND_FLOWCONFIG=12
export EXIT_USAGE=64
export FLOW_LOG_FILE="$_CT_LOG_FILE"
export FLOW_MONITOR_LOG_FILE="$_CT_MONITOR_LOG_FILE"
export FLOW_TEMP_DIR="$_CT_TEMP_DIR"
export IN_FLOW_TEST=1
export FLOW_LOG_LEVEL=debug

# Variables needed by start_flow_unsafe / create_saved_state
saved_state=$_CT_SAVED_STATE
flowlib="$_CT_FLOWLIB"
abs_log_file="$_CT_LOG_FILE"
abs_monitor_log_file="$_CT_MONITOR_LOG_FILE"
wait_for_recheck="$_CT_WAIT_FOR_RECHECK"
file_watcher="$_CT_FILE_WATCHER"
long_lived_workers="$_CT_LONG_LIVED_WORKERS"
THIS_DIR="$_CT_THIS_DIR"

SAVED_OPTION="$(set +o | grep errexit)"

assert_exit_on_line() {
  (
    set -e
    _assert_exit__line=$1; shift
    _assert_exit__ret=0
    _assert_exit__code=$1; shift
    "$@" ||  _assert_exit__ret=$?
    eval "$SAVED_OPTION"
    if [ "$_assert_exit__ret" -eq "$_assert_exit__code" ]; then
      return 0
    else
      echo "\\\`$(basename "$1") \${*:2}\\\` expected to exit code $_assert_exit__code but got $_assert_exit__ret (line $_assert_exit__line)"
      return 1
    fi
  )
  return $?
}
assert_exit() { assert_exit_on_line "\${BASH_LINENO[0]}" "$@"; }
assert_ok() { assert_exit_on_line "\${BASH_LINENO[0]}" "$EXIT_OK" "$@"; }
assert_one() { assert_exit_on_line "\${BASH_LINENO[0]}" "$EXIT_ONE" "$@"; }
assert_errors() { assert_exit_on_line "\${BASH_LINENO[0]}" "$EXIT_ERRS" "$@"; }
assert_server_already_running() { assert_exit_on_line "\${BASH_LINENO[0]}" "$EXIT_SERVER_ALREADY_RUNNING" "$@"; }

query_at_pos() {
  local query=$1 file=$2 line=$3 col=$4
  shift 4
  local flags=("$@")
  printf "%s:%s:%s\\n" "$file" "$line" "$col"
  echo "Flags:" "\${flags[@]}"
  assert_ok "$FLOW" "$query" "$file" "$line" "$col" --strip-root "\${flags[@]}" < "$file"
  printf "\\n"
}

queries_in_file() {
  local query=$1 file=$2
  shift 2
  local arg_flags_array=("$@")
  awk '/^\\/\\/.*\\^/{ print NR }' "$file" | while read -r line; do
    local linep="$line""p"
    local col
    col=$(sed -n "$linep" "$file" | awk -F'^' '{ print $1}' | wc -c)
    col="$((col))"
    local line_flags_array=()
    IFS=" " read -r -a line_flags_array <<< "$(sed -n "$linep" "$file" | awk -F'^' '{ print $2 }')"
    if [ -n "$col" ]; then
      ((line--))
      local all_flags=("\${arg_flags_array[@]}" "\${line_flags_array[@]}")
      query_at_pos "$query" "$file" "$line" "$col" "\${all_flags[@]}"
    fi
  done
}

show_skipping_stats() {
  printf "\\n========Skipping stats========\\n"
  grep -o "Merge skipped [0-9]\\+ of [0-9]\\+ modules" $1 | tail -n 1
  grep -o "Check will skip [0-9]\\+ of [0-9]\\+ files" $1 | tail -n 1
}

create_saved_state() {
  local root="$1"
  local flowconfig_name="$2"
  (
    set -e
    "$FLOW" start "$root" \\
      $flowlib --wait \\
      --wait-for-recheck "$wait_for_recheck" \\
      --lazy-mode none \\
      --file-watcher "$file_watcher" \\
      --flowconfig-name "$flowconfig_name" \\
      --log-file "$abs_log_file" \\
      --monitor-log-file "$abs_monitor_log_file" \\
      --long-lived-workers "$long_lived_workers"

    local SAVED_STATE_FILENAME="$root/.flow.saved_state"
    local CHANGES_FILENAME="$root/.flow.saved_state_file_changes"
    assert_ok "$FLOW" save-state \\
      --root "$root" \\
      --out "$SAVED_STATE_FILENAME" \\
      --flowconfig-name "$flowconfig_name"
    assert_ok "$FLOW" stop --flowconfig-name "$flowconfig_name" "$root"
    touch "$CHANGES_FILENAME"
  ) > /dev/null 2>&1
  return $?
}

start_flow_unsafe() {
  local root=$1; shift
  if [ ! -d "$root" ]; then
    printf "Invalid root directory '%s'\\n" "$root" >&2
    return 1
  fi
  if [[ "$saved_state" -eq 1 ]]; then
    local flowconfig_name=".flowconfig"
    for ((i=1; i<=$#; i++)); do
      opt="\${!i}"
      if [ "$opt" = "--flowconfig-name" ]; then
        ((i++))
        flowconfig_name=\${!i}
      fi
    done
    if create_saved_state "$root" "$flowconfig_name"; then
      PATH="$THIS_DIR/tests_bin:$PATH" \\
      "$FLOW" start "$root" \\
        $flowlib --wait \\
        --wait-for-recheck "$wait_for_recheck" \\
        --saved-state-fetcher "local" \\
        --saved-state-no-fallback \\
        --file-watcher "$file_watcher" \\
        --log-file "$abs_log_file" \\
        --monitor-log-file "$abs_monitor_log_file" \\
        --long-lived-workers "$long_lived_workers" \\
        "$@"
      return $?
    else
      printf "Failed to generate saved state\\n" >&2
      return 1
    fi
  else
    PATH="$THIS_DIR/tests_bin:$PATH" \\
    "$FLOW" start "$root" \\
      $flowlib --wait \\
      --wait-for-recheck "$wait_for_recheck" \\
      --file-watcher "$file_watcher" \\
      --log-file "$abs_log_file" \\
      --monitor-log-file "$abs_monitor_log_file" \\
      --long-lived-workers "$long_lived_workers" \\
      "$@"
    return $?
  fi
}

start_flow() {
  assert_ok start_flow_unsafe "$@"
}

# Auto-start server if configured
auto_start_enabled=$_CT_AUTO_START
if [ "$auto_start_enabled" -eq 1 ]; then
  set +e
  start_flow_unsafe . $_CT_START_ARGS > /dev/null 2>> "$_CT_ERR_FILE"
  code=$?
  set -e
  if [ $code -ne 0 ]; then
    printf "flow start exited code %s\\n" "$code"
    exit $code
  fi
fi

# Run script in subshell (matching original bash behavior where shell
# scripts run in a ( ... ) subshell, so failures don't skip cleanup)
set +e
(
  set -e
  source "$_CT_SCRIPT_PATH" "$FLOW"
)
_script_exit=$?
set -e

# Stop server - always attempt, matching bash's
# "stop server, even if we didn't start it"
"$FLOW" stop . 1> /dev/null 2>&1 || true

exit $_script_exit
`,
        ];

        const result = await execFilePromise(bashCmd[0], bashCmd.slice(1), {
          cwd: workDir,
          env: scriptEnv,
        });
        // The bash script handles its own server start/stop via the
        // inline `"$FLOW" stop . ...` at the end, so the server is
        // already stopped when we get here — no safety cleanup needed.
        serverStarted = false;
        outContent = result.stdout;
        if (config.ignore_stderr) {
          errContent = result.stderr;
        } else {
          outContent += result.stderr;
        }

        if (result.code !== 0) {
          // When auto-start fails, the inline bash script already prints
          // "flow start exited code N" and exits — the original bash
          // runner skips the test script entirely and does NOT print an
          // additional shell exit line.  But when the test script itself
          // fails, the original bash runner DOES print
          // "<shell> exited code N".  Distinguish the two cases by
          // checking whether stdout already contains the start-failure
          // message.
          if (!/flow start exited code/.test(outContent)) {
            outContent += config.shell + ' exited code ' + result.code + '\n';
          }
          returnStatus = RUNTEST_ERROR;
        }
      }
    } else {
      // General cmd mode
      if (config.auto_start) {
        const startArgs = ['start', '.'];
        if (noFlowlib) {
          startArgs.push('--no-flowlib');
        }
        startArgs.push('--wait');

        if (savedState) {
          // Handle saved state start
          let flowconfigName = '.flowconfig';
          const extraStartArgs = config.start_args
            ? splitShellArgs(config.start_args)
            : [];
          for (let i = 0; i < extraStartArgs.length; i++) {
            if (
              extraStartArgs[i] === '--flowconfig-name' &&
              i + 1 < extraStartArgs.length
            ) {
              flowconfigName = extraStartArgs[i + 1];
            }
          }

          // Create saved state first
          const ctx = new TestContext({
            flowBin,
            testDir: workDir,
            logFile,
            monitorLogFile,
            noFlowlib,
            waitForRecheck: config.wait_for_recheck,
            fileWatcher: config.file_watcher,
            longLivedWorkers,
            savedState,
            tempDir: tmpParent,
          });
          const ok = await ctx.createSavedState(workDir, flowconfigName);
          if (!ok) {
            outContent = 'Failed to generate saved state\n';
            returnStatus = RUNTEST_ERROR;
          } else {
            startArgs.push(
              '--wait-for-recheck',
              config.wait_for_recheck,
              '--saved-state-fetcher',
              'local',
              '--saved-state-no-fallback',
              '--file-watcher',
              config.file_watcher,
              '--log-file',
              logFile,
              '--monitor-log-file',
              monitorLogFile,
              '--long-lived-workers',
              longLivedWorkers,
              ...extraStartArgs,
            );
            const ssResult = await execFilePromise(flowBin, startArgs, {
              cwd: workDir,
              env,
            });
            errContent += ssResult.stderr;
            if (ssResult.code !== 0) {
              outContent = 'flow start exited code ' + ssResult.code + '\n';
              returnStatus = RUNTEST_ERROR;
            } else {
              serverStarted = true;
            }
          }
        } else {
          const extraStartArgs = config.start_args
            ? splitShellArgs(config.start_args)
            : [];
          startArgs.push(
            '--wait-for-recheck',
            config.wait_for_recheck,
            '--file-watcher',
            config.file_watcher,
            '--log-file',
            logFile,
            '--monitor-log-file',
            monitorLogFile,
            '--long-lived-workers',
            longLivedWorkers,
            ...extraStartArgs,
          );
          const startResult = await execFilePromise(flowBin, startArgs, {
            cwd: workDir,
            env,
          });
          errContent += startResult.stderr;
          if (startResult.code !== 0) {
            outContent = 'flow start exited code ' + startResult.code + '\n';
            returnStatus = RUNTEST_ERROR;
          } else {
            serverStarted = true;
          }
        }
      }

      if (returnStatus !== RUNTEST_ERROR) {
        // Run the cmd.
        // The bash runner uses `eval "$FLOW $cmd"` which processes shell
        // constructs (pipes, redirections, etc.). Detect metacharacters
        // and use shell execution when needed; otherwise use the safer
        // execFile path.
        const cmdTrimmed = config.cmd.trim();
        const stdinData = config.stdin
          ? await fs.promises.readFile(join(workDir, config.stdin), 'utf8')
          : undefined;

        let result;
        if (/[|;&<>()$`]/.test(cmdTrimmed)) {
          // Shell metacharacters detected — run through shell to match
          // bash `eval "$FLOW $cmd"` semantics.
          const shellCmd =
            '"' + flowBin.replace(/"/g, '\\"') + '" ' + cmdTrimmed;
          result = await execShellPromise(
            shellCmd,
            {cwd: workDir, env},
            stdinData,
          );
        } else {
          const cmdParts = splitShellArgs(cmdTrimmed);
          result = await execFilePromise(
            flowBin,
            cmdParts,
            {cwd: workDir, env},
            stdinData,
          );
        }
        outContent = result.stdout;
        if (config.ignore_stderr) {
          errContent += result.stderr;
        } else {
          outContent += result.stderr;
        }

        // Stop server if one was started
        if (serverStarted) {
          const stopResult = await execFilePromise(flowBin, ['stop', '.'], {
            cwd: workDir,
            env,
          });
          if (stopResult.code === 0) {
            serverStarted = false;
          }
        }
      }
    }

    // Write output files
    await fs.promises.writeFile(outFile, outContent);
    if (errContent) {
      // Use appendFileSync because in bash script mode, the inline script
      // may have already written server stderr to errFile via redirect
      // (2>> "$_CT_ERR_FILE"). writeFileSync would overwrite that output.
      await fs.promises.appendFile(errFile, errContent);
    }

    // Diff output
    if (returnStatus === RUNTEST_SUCCESS) {
      const diffResult = await diffOutput(expDst, outFile, version);
      if (diffResult !== '') {
        await fs.promises.writeFile(diffFile, diffResult);
      }
    }

    // Handle results
    if (returnStatus !== RUNTEST_SUCCESS) {
      // Copy artifacts back to source dir for debugging
      await copyArtifact(outFile, join(testDir, name + '.out'));
      await copyArtifact(logFile, join(testDir, name + '.log'));
      await copyArtifact(monitorLogFile, join(testDir, name + '.monitor_log'));
      await copyArtifact(errFile, join(testDir, name + '.err'));
      return {status: returnStatus, name};
    } else if (await existsAsync(diffFile)) {
      const diffContent = await fs.promises.readFile(diffFile, 'utf8');
      if (diffContent.length > 0) {
        // Test failed - copy artifacts back
        await copyArtifact(outFile, join(testDir, name + '.out'));
        await copyArtifact(logFile, join(testDir, name + '.log'));
        await copyArtifact(
          monitorLogFile,
          join(testDir, name + '.monitor_log'),
        );
        await copyArtifact(errFile, join(testDir, name + '.err'));
        await copyArtifact(diffFile, join(testDir, name + '.diff'));

        if (record) {
          await recordOutput(outFile, join(testDir, expFileName), version);
          // Clean up after recording
          await cleanArtifact(join(testDir, name + '.out'));
          await cleanArtifact(join(testDir, name + '.err'));
          await cleanArtifact(join(testDir, name + '.diff'));
        }

        return {status: RUNTEST_FAILURE, name, diff: diffContent};
      }
    }

    // Success - clean up artifacts from previous runs
    await cleanArtifact(join(testDir, name + '.out'));
    await cleanArtifact(join(testDir, name + '.log'));
    await cleanArtifact(join(testDir, name + '.monitor_log'));
    await cleanArtifact(join(testDir, name + '.err'));
    await cleanArtifact(join(testDir, name + '.diff'));

    return {status: RUNTEST_SUCCESS, name};
  } finally {
    // Clean up temp directory
    // Stop any running flow server in the working directory.
    // Only attempt this if a server was actually started, to avoid
    // spawning a useless process for every full-check test.
    if (serverStarted) {
      // Use a short timeout — this is best-effort cleanup and shouldn't
      // block the runner if the server is unresponsive.
      try {
        await execFilePromise(flowBin, ['stop', '.'], {
          cwd: workDir,
          env,
          timeout: 30 * 1000,
        });
      } catch (e) {
        // Ignore server stop errors
      }
    }
    try {
      await require('fs').promises.rm(tmpParent, {
        recursive: true,
        force: true,
      });
    } catch (e) {
      // Ignore cleanup errors
    }
  }
}

async function copyArtifact(src: string, dst: string): Promise<void> {
  try {
    if (await existsAsync(src)) {
      const content = await fs.promises.readFile(src);
      if (content.length > 0) {
        await fs.promises.writeFile(dst, content);
      } else {
        // Source is empty — clean the destination to match bash's
        // `[ -s file ] && mv file dest || rm -f dest/file` pattern.
        await cleanArtifact(dst);
      }
    } else {
      // Source doesn't exist — clean old artifact from destination.
      await cleanArtifact(dst);
    }
  } catch (e) {
    // Ignore
  }
}

async function cleanArtifact(filePath: string): Promise<void> {
  try {
    if (await existsAsync(filePath)) {
      await fs.promises.unlink(filePath);
    }
  } catch (e) {
    // Ignore
  }
}

module.exports = {
  runOneTest,
  RUNTEST_SUCCESS,
  RUNTEST_FAILURE,
  RUNTEST_SKIP,
  RUNTEST_MISSING_FILES,
  RUNTEST_ERROR,
  RUNTEST_MISSING_ALL_OPTION,
};
