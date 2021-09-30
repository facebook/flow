/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const colors = require('colors/safe');

const {format} = require('util');
const {basename, dirname, resolve, join} = require('path');
const {spawn} = require('child_process');

const {getTestsDir} = require('../constants');
const {drain, readFile, rimraf, symlink} = require('../utils/async');
const {Builder} = require('./builder');
const {findTestsByName, findTestsByRun, loadSuite} = require('./findTests');
const {RunQueue} = require('./RunQueue');

import type {Suite} from './Suite';
import type {Args} from './testCommand';

/* We potentially have a ton of info to dump into stdout/stderr. Let's handle
 * drain events properly */
async function write(
  writer: stream$Writable | tty$WriteStream,
  fmt: string,
  ...args: Array<mixed>
): Promise<void> {
  if (false == writer.write(format(fmt, ...args))) {
    await drain(writer);
  }
}

function startWatchAndRun(suites, args) {
  // Require in here to avoid the dependency for most test runs
  const sane = require('sane');

  let running = false;
  let queuedSet = new Set();
  let changedThings = new Set();
  let queuedMessages = [];

  process.stderr.write(format('Watching %d suites\n', suites.size));

  let shortcuts: Map<string, [string, () => Promise<mixed>]> = new Map();

  const printShortcuts = () => {
    process.stdout.write('\nShortcuts:\n');
    for (const [char, [descr, _]] of shortcuts.entries()) {
      process.stdout.write(format('%s    %s\n', char, descr));
    }
    process.stdout.write('> ');
  };

  const keydown = chunk => {
    const char = chunk.toString()[0];

    const shortcut = shortcuts.get(char);

    if (shortcut) {
      const [name, fn] = shortcut;
      process.stdout.write(format('%s\n\n', name));
      fn();
    } else {
      process.stdout.write(format('Unknown shortcut: %s\n', char));
      printShortcuts();
    }
  };

  const startListeningForShortcuts = () => {
    // $FlowFixMe[prop-missing]
    // $FlowFixMe[method-unbinding]
    if (typeof process.stdin.setRawMode === 'function') {
      (process.stdin.setRawMode: any).call(process.stdin, true);
      process.stdin.resume();
      process.stdin.setEncoding('utf8');
      process.stdin.on('data', keydown);

      printShortcuts();
    }
  };

  const stopListeningForShortcuts = () => {
    // $FlowFixMe[prop-missing]
    // $FlowFixMe[method-unbinding]
    if (typeof process.stdin.setRawMode === 'function') {
      (process.stdin.setRawMode: any).call(process.stdin, false);
      process.stdin.resume();
      process.stdin.setEncoding('utf8');
      process.stdin.removeListener('data', keydown);
    }
  };

  const run = async () => {
    if (running === true) {
      return;
    }

    if (changedThings.size > 0) {
      process.stderr.write('\n!!!!!!!!!!!!!!!!\n');
      for (const changedThing of changedThings) {
        process.stderr.write(
          format('Watcher noticed that %s changed\n', changedThing),
        );
      }
    }
    changedThings = new Set();

    if (queuedSet.size === 0) {
      return;
    }

    running = true;
    stopListeningForShortcuts();
    const runSet = queuedSet;
    queuedSet = new Set();

    const suitesToRun = {};
    for (const suiteName of runSet) {
      try {
        suitesToRun[suiteName] = loadSuite(suiteName);
      } catch (e) {
        process.stderr.write(
          format(
            colors.red.bold('Failed to load test suite `%s`\n%s\n'),
            colors.blue(suiteName),
            e.stack,
          ),
        );
      }
    }

    if (Object.keys(suitesToRun).length > 0) {
      const [exitCode, runID] = await runOnce(suitesToRun, args);

      shortcuts.set('t', [
        'rerun the tests you just ran',
        () => rerun(runID, false),
      ]);
      if (exitCode) {
        shortcuts.set('f', [
          'rerun only the tests that just failed',
          () => rerun(runID, true),
        ]);
        shortcuts.set('r', [
          'record the tests that just failed',
          () => record(runID),
        ]);
      } else {
        shortcuts.delete('f');
        shortcuts.delete('r');
      }
    }

    running = false;
    startListeningForShortcuts();
    run();
  };

  async function rerun(runID, failedOnly) {
    suites = await findTestsByRun(runID, failedOnly);
    suites.forEach(suite => queuedSet.add(suite));
    run();
  }

  async function record(runID) {
    running = true;
    stopListeningForShortcuts();

    const child = spawn(
      process.argv[0],
      [process.argv[1], 'record', '--rerun-failed', runID],
      {stdio: 'inherit'},
    );
    const code = await new Promise((resolve, reject) => {
      child.on('close', resolve);
    });

    if (code === 0) {
      shortcuts.delete('f');
      shortcuts.delete('r');
    }
    startListeningForShortcuts();
    running = false;
  }

  const watch = (watcher, name, suiteNames) => {
    const callback = () => {
      changedThings.add(name);
      suiteNames.forEach(suite => queuedSet.add(suite));

      // We may have a lot of FS events...wait for things to settle
      setTimeout(run, 500);
    };
    watcher.on('change', callback);
    watcher.on('add', callback);
    watcher.on('delete', callback);
  };

  shortcuts.set('q', ['quit', async () => process.exit(0)]);
  shortcuts.set('a', [
    'run all the tests',
    async () => {
      suites.forEach(suiteName => queuedSet.add(suiteName));
      run();
    },
  ]);
  startListeningForShortcuts();

  watch(
    sane(dirname(args.bin), {glob: [basename(args.bin)]}),
    'the Flow binary',
    Array.from(suites),
  );
  for (const suite of suites) {
    const suiteDir = resolve(getTestsDir(), suite);
    watch(sane(suiteDir), format('the `%s` suite', suite), [suite]);
  }
}

async function runOnce(suites: {[suiteName: string]: Suite}, args) {
  let maxErroredTests = 0;
  if (args.maxErroredTests != null) {
    maxErroredTests = args.maxErroredTests;
  }
  if (args.maxErroredTestsPct != null) {
    const numTests = Object.keys(suites).length;
    maxErroredTests = Math.floor((numTests * args.maxErroredTestsPct) / 100);
  }
  if (maxErroredTests > 0) {
    process.stderr.write(
      format(
        'A maximum of %d suite%s allowed to error\n',
        maxErroredTests,
        maxErroredTests === 1 ? ' is' : 's are',
      ),
    );
  }

  const builder = new Builder(args.errorCheckCommand);
  const runQueue = new RunQueue(
    args.bin,
    args.parallelism,
    args.fbmakeJson,
    suites,
    builder,
  );

  await runQueue.go();
  await builder.cleanup();

  const results = runQueue.results;
  // 1 - At least one test failed
  // 2 - No tests failed but too many tests errored
  // 0 - No tests failed and there were an acceptable number of errored tests
  let exitCode = 0;
  for (const suiteName of Object.keys(results).sort()) {
    const suiteResult = results[suiteName];
    if (suiteResult.type === 'exceptional') {
      maxErroredTests--;
      if (maxErroredTests < 0 && exitCode === 0) {
        exitCode = 2;
      }
      await write(
        process.stdout,
        colors.bgRed(colors.white.bold('ERRORED')) +
          colors.grey(': suite ') +
          colors.blue('%s') +
          '\n' +
          colors.red('%s') +
          '\n',
        suiteName,
        suiteResult.message,
      );
    } else {
      const {testResults} = suiteResult;
      for (let testNum = 0; testNum < testResults.length; testNum++) {
        const testResult = testResults[testNum];
        for (
          let stepNum = 0;
          stepNum < testResult.stepResults.length;
          stepNum++
        ) {
          const result = testResult.stepResults[stepNum];
          if (!result.passed) {
            exitCode = 1;
            await write(
              process.stdout,
              colors.red.bold('FAILED') +
                colors.grey(': suite ') +
                colors.blue('%s') +
                colors.grey(', test ') +
                colors.blue('%s') +
                colors.grey(' (%d of %d), step %d of %d') +
                '\n',
              suiteName,
              testResult.name || 'unnamed test',
              testNum + 1,
              testResults.length,
              stepNum + 1,
              testResult.stepResults.length,
            );
            const messages = [];
            if (result.exception !== undefined) {
              messages.push(format('Uncaught exception: %s', result.exception));
            }
            for (const assertionResult of result.assertionResults) {
              if (assertionResult.type === 'fail') {
                messages.push(...assertionResult.messages);
              }
            }
            for (const message of messages) {
              await write(process.stdout, '%s\n', message);
            }
          }
        }
        const didTestFail = testResult.stepResults.some(r => !r.passed);
        if (didTestFail) {
          const logFile = join(
            builder.baseDirForSuite(suiteName),
            'tmp',
            String(testNum + 1),
            'test.log',
          );
          const logCommand =
            process.env['VISUAL'] || process.env['EDITOR'] || 'cat';
          let logContents = '';
          try {
            logContents = await readFile(logFile);
          } catch (e) {
            logContents = e.message != null ? e.message : String(e);
          }
          await write(
            process.stdout,
            colors.grey.bold('%s %s') + '\n' + colors.grey('%s') + '\n',
            logCommand,
            logFile,
            logContents,
          );
        }
      }
    }
  }

  const runID = builder.runID;
  const nextSteps: Array<[string, string]> = [
    ['Rerun the tests you just ran', `./tool test --rerun ${runID}`],
  ];

  if (exitCode != 0) {
    nextSteps.push([
      'Rerun the tests that just failed',
      `./tool test --rerun-failed ${runID}`,
    ]);
    nextSteps.push([
      'Record the tests that just failed',
      `./tool record --rerun-failed ${runID}`,
    ]);
  }

  await write(process.stderr, 'Possible next steps:');
  for (const [descr, cmd] of nextSteps) {
    await write(
      process.stderr,
      `
  ${descr}
    ${cmd}`,
    );
  }
  await write(process.stderr, '\n\n');

  return [exitCode, runID];
}

async function testRunner(args: Args): Promise<void> {
  process.env.IN_FLOW_TEST = '1';
  if (!process.env.hasOwnProperty('FLOW_MAX_WORKERS')) {
    process.env.FLOW_MAX_WORKERS = '2';
  }

  await write(process.stderr, `Using flow binary: ${args.bin}\n`);

  if (args.buckCpTestsDir != null) {
    const src = args.buckCpTestsDir;
    const dest = getTestsDir();

    await rimraf(dest);
    await symlink(src, dest);
  }

  let suites;
  if (args.rerun != null) {
    suites = await findTestsByRun(args.rerun, args.failedOnly);
  } else {
    suites = await findTestsByName(args.suites);
  }

  if (args.watch) {
    startWatchAndRun(suites, args);
  } else {
    const loadedSuites = {};
    for (const suiteName of suites) {
      loadedSuites[suiteName] = loadSuite(suiteName);
    }
    if (Object.keys(loadedSuites).length > 0) {
      const [exitCode, _] = await runOnce(loadedSuites, args);
      process.exit(exitCode);
    } else {
      process.stderr.write('No suites to run\n');
      process.exit(1);
    }
  }
}

module.exports = {
  default: testRunner,
};
