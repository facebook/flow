/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {execFileSync} = require('child_process');
const {readdirSync, statSync, existsSync, readFileSync} = require('fs');
const {join, resolve, basename} = require('path');
const {format} = require('util');

const {
  runOneTest,
  RUNTEST_SUCCESS,
  RUNTEST_FAILURE,
  RUNTEST_SKIP,
  RUNTEST_MISSING_FILES,
  RUNTEST_ERROR,
  RUNTEST_MISSING_ALL_OPTION,
} = require('./checkRunOneTest');
const {CheckRunQueue} = require('./checkRunQueue');

export type RunnerArgs = {
  bin: string,
  testsDir: string,
  filter: ?string,
  parallelism: number,
  checkOnly: boolean,
  savedState: boolean,
  longLivedWorkers: boolean,
  record: boolean,
  quiet: boolean,
  verbose: boolean,
  jsonOutput: boolean,
  listTests: boolean,
};

const COLOR_RESET = '\x1b[0m';
const COLOR_DEFAULT = '\x1b[39;49;0m';
const COLOR_DEFAULT_BOLD = '\x1b[39;49;1m';
const COLOR_RED_BOLD = '\x1b[31;1m';
const COLOR_GREEN_BOLD = '\x1b[32;1m';
const COLOR_YELLOW_BOLD = '\x1b[33;1m';
const COLOR_MAGENTA_BOLD = '\x1b[35;1m';
const COLOR_WHITE_ON_RED_BOLD = '\x1b[37;41;1m';

function useColor(): boolean {
  // $FlowFixMe[prop-missing] - isTTY exists on process.stdout at runtime
  return Boolean(process.stdout.isTTY);
}

function color(code: string): string {
  return useColor() ? code : '';
}

async function checkTestRunner(args: RunnerArgs): Promise<void> {
  const {
    bin,
    testsDir,
    filter,
    parallelism,
    checkOnly,
    savedState,
    longLivedWorkers,
    record,
    quiet,
    verbose,
    jsonOutput,
    listTests,
  } = args;

  // Set environment variables needed by tests. Save originals so we can
  // restore them when done, avoiding permanent process.env mutation.
  const savedEnv: {[string]: string | void} = {};
  function setEnvIfNeeded(key: string, value: string, onlyIfUnset?: boolean) {
    if (onlyIfUnset && process.env[key] !== undefined) return;
    savedEnv[key] = process.env[key];
    process.env[key] = value;
  }
  setEnvIfNeeded('IN_FLOW_TEST', '1');
  setEnvIfNeeded('FLOW_LOG_LEVEL', 'debug');
  setEnvIfNeeded('FLOW_MAX_WORKERS', '2', true);

  // Set git binary
  if (process.env.FLOW_GIT_BINARY === undefined) {
    try {
      const cmd = process.platform === 'win32' ? 'where' : 'which';
      const gitPath = String(
        execFileSync(cmd, ['git'], {encoding: 'utf8'}),
      ).trim();
      if (gitPath) {
        // On Windows, `where` can return multiple lines; take the first.
        setEnvIfNeeded('FLOW_GIT_BINARY', gitPath.split('\n')[0].trim());
      }
    } catch (e) {
      // git not available
    }
  }

  // Set node binary
  if (!process.env.FLOW_NODE_BINARY) {
    setEnvIfNeeded(
      'FLOW_NODE_BINARY',
      process.env.NODE_BINARY || process.execPath,
    );
  }

  // Get version
  let version: string;
  try {
    version = String(
      execFileSync(bin, ['version', '--semver'], {encoding: 'utf8'}),
    ).trim();
  } catch (e) {
    process.stderr.write(
      format('Failed to get Flow version from %s: %s\n', bin, e.message),
    );
    process.exit(1);
    return; // unreachable, for flow
  }

  // Discover test directories
  const testsDirResolved = resolve(testsDir);
  let testDirs: Array<string>;
  try {
    testDirs = readdirSync(testsDirResolved)
      .filter(entry => {
        try {
          return statSync(join(testsDirResolved, entry)).isDirectory();
        } catch (e) {
          return false;
        }
      })
      .sort()
      .map(entry => join(testsDirResolved, entry));
  } catch (e) {
    process.stderr.write(
      format(
        'Failed to read tests directory %s: %s\n',
        testsDirResolved,
        e.message,
      ),
    );
    process.exit(1);
    return;
  }

  // Apply filter
  if (filter) {
    let filterRe: RegExp;
    try {
      filterRe = new RegExp(filter);
    } catch (e) {
      process.stderr.write(
        format('Invalid filter regex "%s": %s\n', filter, e.message),
      );
      process.exit(1);
      return;
    }
    testDirs = testDirs.filter(dir => filterRe.test(basename(dir)));
  }

  // List mode
  if (listTests) {
    for (const dir of testDirs) {
      let name = basename(dir);
      if (savedState) {
        name += '-saved-state';
      } else if (longLivedWorkers) {
        name += '-long-lived-workers';
      }
      process.stdout.write(name + '\n');
    }
    return;
  }

  if (!quiet) {
    process.stdout.write(
      format('Running up to %d test(s) in parallel\n', parallelism),
    );
  }

  // Create jobs
  const jobs = testDirs.map((dir, index) => ({
    index,
    testDir: dir,
    run: () =>
      runOneTest({
        testDir: dir,
        flowBin: bin,
        version,
        checkOnly,
        savedState,
        longLivedWorkers,
        record,
      }),
  }));

  // Run tests
  const queue = new CheckRunQueue(jobs, {parallelism, quiet});
  const results = await queue.run();

  // Build results map by test name for robust matching
  // (avoids assuming results array order matches job index order)
  const resultsByName: Map<string, (typeof results)[0]> = new Map();
  for (const result of results) {
    resultsByName.set(result.name, result);
  }

  // Print results in order
  let passed = 0;
  let failed = 0;
  let skipped = 0;
  let errored = 0;
  const jsonMap: {[string]: boolean} = {};

  for (let i = 0; i < testDirs.length; i++) {
    const testName = basename(testDirs[i]);
    const result = resultsByName.get(testName);

    if (!result) {
      errored++;
      if (jsonOutput) {
        jsonMap[testName] = false;
      } else if (!quiet) {
        process.stdout.write(
          format(
            '%s[✗] ERRORED:%s %s%s\n',
            color(COLOR_RED_BOLD),
            color(COLOR_DEFAULT),
            testName,
            color(COLOR_RESET),
          ),
        );
      }
      continue;
    }

    switch (result.status) {
      case RUNTEST_SUCCESS:
        passed++;
        if (jsonOutput) {
          jsonMap[testName] = true;
        } else if (!quiet) {
          process.stdout.write(
            format(
              '%s[✓] PASSED:%s  %s%s\n',
              color(COLOR_GREEN_BOLD),
              color(COLOR_DEFAULT),
              testName,
              color(COLOR_RESET),
            ),
          );
        }
        break;

      case RUNTEST_FAILURE:
        failed++;
        if (jsonOutput) {
          jsonMap[testName] = false;
        } else {
          if (record) {
            process.stdout.write(
              format(
                '%s[✗] UPDATED:%s %s%s\n',
                color(COLOR_MAGENTA_BOLD),
                color(COLOR_DEFAULT),
                testName,
                color(COLOR_RESET),
              ),
            );
          } else {
            process.stdout.write(
              format(
                '%s[✗] FAILED:%s  %s%s\n',
                color(COLOR_RED_BOLD),
                color(COLOR_DEFAULT),
                testName,
                color(COLOR_RESET),
              ),
            );
          }

          // Print diff
          const errPath = join(testDirs[i], testName + '.err');
          if (existsSync(errPath)) {
            process.stdout.write(readFileSync(errPath, 'utf8'));
          }
          // Use result.diff directly (more reliable than reading from file,
          // which may have been cleaned up in record mode)
          if (result.diff) {
            let diffContent = result.diff;
            if (useColor()) {
              diffContent = diffContent
                .split('\n')
                .map(line => {
                  if (line.startsWith('-'))
                    return '\x1b[31m' + line + '\x1b[0m';
                  if (line.startsWith('+'))
                    return '\x1b[32m' + line + '\x1b[0m';
                  if (line.startsWith('@'))
                    return '\x1b[35m' + line + '\x1b[0m';
                  return line;
                })
                .join('\n');
            }
            process.stdout.write(diffContent);
          }
        }
        break;

      case RUNTEST_SKIP:
        skipped++;
        if (!jsonOutput && !quiet) {
          if (verbose) {
            process.stdout.write(
              format(
                '%s[-] SKIPPED:%s %s%s\n',
                color(COLOR_YELLOW_BOLD),
                color(COLOR_DEFAULT),
                testName,
                color(COLOR_RESET),
              ),
            );
          }
        }
        break;

      case RUNTEST_MISSING_FILES:
        errored++;
        if (jsonOutput) {
          jsonMap[testName] = false;
        } else if (!quiet) {
          process.stdout.write(
            format(
              '%s[✗] ERRORED:%s %s%s\n',
              color(COLOR_RED_BOLD),
              color(COLOR_DEFAULT),
              testName,
              color(COLOR_RESET),
            ),
          );
          process.stdout.write(
            format('Missing %s.exp file or .flowconfig file\n', testName),
          );
        }
        break;

      case RUNTEST_MISSING_ALL_OPTION:
        errored++;
        if (jsonOutput) {
          jsonMap[testName] = false;
        } else if (!quiet) {
          process.stdout.write(
            format(
              '%s[✗] ERRORED:%s %s%s\n',
              color(COLOR_RED_BOLD),
              color(COLOR_DEFAULT),
              testName,
              color(COLOR_RESET),
            ),
          );
          process.stdout.write(
            'You are required to set either `all=true` or `all=false` in your test `.flowconfig`.\n',
          );
        }
        break;

      case RUNTEST_ERROR:
      default:
        errored++;
        if (jsonOutput) {
          jsonMap[testName] = false;
        } else if (!quiet) {
          process.stdout.write(
            format(
              '%s[✗] ERRORED:%s %s%s\n',
              color(COLOR_RED_BOLD),
              color(COLOR_DEFAULT),
              testName,
              color(COLOR_RESET),
            ),
          );
          // Print error artifacts
          const outPath = join(testDirs[i], testName + '.out');
          if (existsSync(outPath)) {
            process.stdout.write(readFileSync(outPath, 'utf8'));
          }
          const errFilePath = join(testDirs[i], testName + '.err');
          if (existsSync(errFilePath)) {
            process.stdout.write('\n\nStderr:\n');
            process.stdout.write(readFileSync(errFilePath, 'utf8'));
          }
          const monLogPath = join(testDirs[i], testName + '.monitor_log');
          if (existsSync(monLogPath)) {
            process.stdout.write('\n\nServer monitor log:\n');
            process.stdout.write(readFileSync(monLogPath, 'utf8'));
          }
          const logPath = join(testDirs[i], testName + '.log');
          if (existsSync(logPath)) {
            process.stdout.write('\n\nServer log:\n');
            process.stdout.write(readFileSync(logPath, 'utf8'));
          }
        }
        break;
    }
  }

  // JSON output
  if (jsonOutput) {
    process.stdout.write(JSON.stringify(jsonMap) + '\n');
  }

  // Summary
  if (!quiet) {
    const failedColor =
      failed > 0 ? color(COLOR_WHITE_ON_RED_BOLD) : color(COLOR_DEFAULT_BOLD);
    const erroredColor =
      errored > 0 ? color(COLOR_WHITE_ON_RED_BOLD) : color(COLOR_DEFAULT_BOLD);
    process.stdout.write('\n');
    process.stdout.write(
      format(
        '%sPassed: %d, %sFailed: %d%s, Skipped: %d, %sErrored: %d%s\n',
        color(COLOR_DEFAULT_BOLD),
        passed,
        failedColor,
        failed,
        color(COLOR_DEFAULT_BOLD),
        skipped,
        erroredColor,
        errored,
        color(COLOR_RESET),
      ),
    );
  }

  if (failed > 0 || errored > 0) {
    // Use exitCode instead of process.exit() to allow stdout to flush
    // completely (process.exit can truncate buffered output when piped).
    process.exitCode = 1;
  }

  // Restore environment variables to their original values.
  for (const key of Object.keys(savedEnv)) {
    if (savedEnv[key] === undefined) {
      delete process.env[key];
    } else {
      process.env[key] = savedEnv[key];
    }
  }
}

module.exports = {checkTestRunner};
