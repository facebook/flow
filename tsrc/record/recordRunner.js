/* @flow */

import colors from 'colors/safe';
import {join} from 'path';
import {format} from 'util';

import {readFile, writeFile} from './../async';
import Builder from '../test/builder';
import findTests from '../test/finder';
import parser from 'flow-parser';
import RunQueue from '../test/RunQueue';
import {testsDir} from '../constants';

import type {Args} from './recordCommand';

function escapeString(str: string): string {
  return str
    .replace(/`/g, '\\`')
    .replace(/\$/g, '\\$');
}

function suggestionToString(suggestion): string {
  return format(
    "%s(%s%s)",
    suggestion.method,
    suggestion.args.length === 0 ? "" : "\n",
    suggestion.args.map(arg => {
      switch(typeof arg) {
        case "string":
          return format("`%s`,\n", escapeString(arg));
        case "number":
          return format("%d,\n", arg);
        case "object":
          return format("%s,\n", JSON.stringify(arg, null, 2));
        default:
          new Error("Unhandled arg type");
      }
    }),
  );
}

function dfsForRange(node, line, col): ?[number, number] {
  const todo = [];
  if (typeof node === "object" && node != null && node.hasOwnProperty("type")) {
    if (node.type === "CallExpression") {
      if (node.callee.type === "MemberExpression") {
        if (node.callee.property.loc.start.line === line &&
            node.callee.property.loc.start.column === col-1) {
          return [
            node.callee.property.range[0],
            node.range[1],
          ];
        }
      }
    }
    for (var prop in node) {
      todo.push(node[prop]);
    }
  } else if (Array.isArray(node)) {
    todo.push(...node);
  }

  for (const child of todo) {
    const ret = dfsForRange(child, line, col);
    if (ret != null) {
      return ret;
    }
  }
  return null;
}

export default async function(args: Args): Promise<void> {
  const builder = new Builder(args.errorCheckCommand);
  const suites = await findTests(args.suites);

  const runQueue = new RunQueue(args.bin, args.parallelism, false, suites, builder);

  await runQueue.go();

  let totalTests, totalSteps, testNum, stepNum, suiteName;

  function printStatus(status: 'RECORDING' | 'RECORDED' | 'FAIL'): void {
    let statusText = colors.bold("[ ] RECORDING:");
    let newline = "";
    if (status === 'RECORDED') {
      statusText = colors.green.bold("[✓] RECORDED:")
      newline = "\n";
    } else if (status === 'FAIL') {
      statusText = colors.red.bold("[✗] FAIL:")
      newline = "\n";
    }
    if (process.stdout.isTTY) {
      // $FlowFixMe - Add this to lib file
      process.stdout.clearLine();
      process.stdout.write(format(
        "\r%s  %s (%d/%d tests %d/%d steps passed)%s",
        statusText,
        suiteName,
        testNum,
        totalTests,
        stepNum,
        totalSteps,
        newline,
      ));
    } else {
      if (status == 'FAIL' || status == 'RECORDED') {
        process.stdout.write(format(
          "%s  %s (%d/%d tests %d/%d steps passed)\n",
          statusText,
          suiteName,
          testNum,
          totalTests,
          stepNum,
          totalSteps,
        ));
      }
    }
  }

  const results = runQueue.results;
  for (suiteName in results) {
    // TODO - reorder records based on line number
    totalTests = results[suiteName].length;
    for (testNum = 0; testNum < totalTests; testNum++) {
      let testFailed = false;
      let testRecorded = false;
      totalSteps = results[suiteName][totalTests - testNum - 1].stepResults.length;
      for (stepNum = 0; stepNum < totalSteps; stepNum++) {
        printStatus("RECORDING")
        // Record starting at the end to avoid messing with line numbers
        const stepResult = results[suiteName][totalTests - testNum - 1].stepResults[totalSteps - stepNum - 1];
        if(!stepResult.passed) {
          // Again, start with the last assertion
          for (let assertionNum = stepResult.assertionResults.length - 1; assertionNum >= 0; assertionNum--) {
            const result = stepResult.assertionResults[assertionNum];
            if (result.type === "fail") {
              const assertLoc = result.assertLoc;
              if (assertLoc) {
                const filename = join(testsDir, suiteName, assertLoc.filename);
                const code = await readFile(filename);
                const ast = parser.parse(code, {});
                const range = assertLoc && dfsForRange(ast, assertLoc.line, assertLoc.column);
                if (range) {
                  const [start, end] = range;
                  const out =
                    code.slice(0, start) +
                    suggestionToString(result.suggestion) +
                    code.slice(end);
                  await writeFile(filename, out);
                } else {
                  process.stderr.write("Could not find the assertion in the code\n");
                }
              } else {
                process.stderr.write("Could not find the assertion in the stack\n");
              }
            }
          }
        }
      }
    }
    printStatus("RECORDED");
  }
}
