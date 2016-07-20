/* @flow */

import colors from 'colors/safe';

import {format} from 'util';
import {basename, dirname, resolve} from 'path';

import {testsDir} from '../constants';
import {drain} from '../async';
import Builder from './builder';
import {findTestsByName, findTestsByRun, loadSuite} from './findTests';
import RunQueue from './RunQueue';

import type {Args} from './testCommand';

/* We potentially have a ton of info to dump into stdout. Let's handle drain
 * events properly */
async function write(fmt: string, ...args: Array<mixed>): Promise<void> {
  if (false == process.stdout.write(format(fmt, ...args))) {
    await drain(process.stdout);
  }
}

function startWatchAndRun(suites, args) {
  // Require in here to avoid the dependency for most test runs
  const sane = require('sane');

  let running = false;
  let queuedSet = new Set();
  let queuedMessages = [];

  process.stderr.write(
    format("Watching %d suites\n", Object.keys(suites).length),
  );

  const run = async () => {
    if (running === true) {
      return;
    }

    queuedMessages.forEach(msg => process.stderr.write(msg + "\n"));
    queuedMessages = [];

    if (queuedSet.size === 0) {
      return;
    }
    running = true;
    const runSet = queuedSet;
    queuedSet = new Set();

    const suitesToRun = {};
    for (const suite of runSet) {
      suitesToRun[suite] = suites[suite];
    }

    await runOnce(suitesToRun, args);

    running = false;
    run();
  }

  const watch = (watcher, name, suiteNames) => {
    const callback = () => {
      if (queuedSet.size === 0) {
        queuedMessages.push("\n!!!!!!!!!!!!!!!!");
      }
      queuedMessages.push(format("Watcher noticed that %s changed", name));
      suiteNames.forEach(suite => {
        try {
          suites[suite] = loadSuite(suite);
          queuedSet.add(suite);
        } catch (e) {
          queuedMessages.push(format(
            colors.red.bold("Failed to load test suite `%s`\n%s"),
            colors.blue(suite),
          ));
          queuedMessages.push(e.stack);
        }
        setTimeout(run, 500); // Wait for it to settle;
      });
    }
    watcher.on('change', callback);
    watcher.on('add', callback);
    watcher.on('delete', callback);
  };


  watch(
    sane(dirname(args.bin), {glob: [basename(args.bin)]}),
    "the Flow binary",
    Object.keys(suites),
  );
  for (const suite in suites) {
    const suiteDir = resolve(testsDir, suite);
    watch(sane(suiteDir), format("the `%s` suite", suite), [suite]);
  }
}

async function runOnce(suites, args) {
  const builder = new Builder(args.errorCheckCommand);
  const runQueue = new RunQueue(
    args.bin,
    args.parallelism,
    args.fbmakeJson,
    suites,
    builder,
  );

  await runQueue.go();

  const results = runQueue.results;
  let exitCode = 0;
  for (const suiteName of Object.keys(results).sort()) {
    for (let testNum = 0; testNum < results[suiteName].length; testNum++) {
      const testResult = results[suiteName][testNum];
      for (let stepNum = 0; stepNum < testResult.stepResults.length; stepNum++) {
        const result = testResult.stepResults[stepNum];
        if(!result.passed) {
          exitCode = 1;
          await write(
            colors.red.bold("FAILED")+
              colors.grey(": suite ")+
              colors.blue("%s")+
              colors.grey(", test ")+
              colors.blue("%s")+
              colors.grey(" (%d of %d), step %d of %d") + "\n",
            suiteName,
            testResult.name || "unnamed test",
            testNum+1,
            results[suiteName].length,
            stepNum+1,
            testResult.stepResults.length,
          );
          const messages = [];
          if (result.exception !== undefined) {
            messages.push(format("Uncaught exception: %s", result.exception));
          }
          for (const assertionResult of result.assertionResults) {
            if (assertionResult.type === "fail") {
              messages.push(...assertionResult.messages);
            }
          }
          for (const message of messages) {
            await write("%s\n", message);
          }
        }
      }
    }
  }

  const runID = builder.runID;
  const nextSteps: Array<[string, string]> = [
    [
      "Rerun the tests you just ran",
      `./tool test --rerun ${runID}`,
    ],
  ];

  if (exitCode != 0) {
    nextSteps.push(
      [
        "Rerun the tests that just failed",
        `./tool test --rerun-failed ${runID}`,
      ],
    );
    nextSteps.push(
      [
        "Record the tests that just failed",
        `./tool record --rerun-failed ${runID}`,
      ],
    );
  };

  process.stderr.write("Possible next steps:");
  for (const [descr, cmd] of nextSteps) {
    process.stderr.write(`
  ${descr}
    ${cmd}`);
  }
  process.stderr.write("\n\n");

  return exitCode;
}

export default async function(args: Args): Promise<void> {
  let suites;
  if (args.rerun != null) {
    suites = await findTestsByRun(args.rerun, args.failedOnly);
  } else {
    suites = await findTestsByName(args.suites);
  }

  if (args.watch) {
    startWatchAndRun(suites, args);
  } else {
    const exitCode = await runOnce(suites, args);
    process.exit(exitCode);
  }
}
