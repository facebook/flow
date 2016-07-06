/* @flow */

import colors from 'colors/safe';

import {format} from 'util';

import {drain} from '../async';
import Builder from './builder';
import {findTestsByName, findTestsByRun} from './findTests';
import RunQueue from './RunQueue';

import type {Args} from './testCommand';

/* We potentially have a ton of info to dump into stdout. Let's handle drain
 * events properly */
async function write(fmt: string, ...args: Array<mixed>): Promise<void> {
  if (false == process.stdout.write(format(fmt, ...args))) {
    await drain(process.stdout);
  }
}

export default async function(args: Args): Promise<void> {
  const builder = new Builder(args.errorCheckCommand);

  let suites;
  if (args.rerun != null) {
    suites = await findTestsByRun(args.rerun, args.failedOnly);
  } else {
    suites = await findTestsByName(args.suites);
  }

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

  process.exit(exitCode);
}
