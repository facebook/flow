/* @flow */

import { dirname, relative, resolve } from 'path';
import { format } from 'util';

import { exec } from '../async';
import Suite from './Suite';
import {testsDir} from '../constants';

import type {Tests} from './Tester';

const testSuiteRegex = /(.*)\/test.js/;

async function findTestSuites(): Promise<Array<string>> {
  const cmd = format(
    'find -H %s -name "test.js"',
    testsDir,
  );
  const stdout = await exec(cmd, {cwd: __dirname});
  return stdout.trim().split("\n");
}

export default async function(suites: ?Set<string>): Promise<{ [key: string]: Suite }> {
  if (suites != null) {
    // Make a copy before messing with it
    suites = new Set(suites);
    for (const suite of suites) {
      suites.add(resolve(suite));
    }
  }
  const testSuites = await findTestSuites();

  const result = {};

  process.stderr.write(format("Found %d suites\n", testSuites.length));
  for (const suiteFile of testSuites) {
    const suiteName = relative(testsDir, suiteFile).replace(testSuiteRegex, "$1");
    const pathToDir = dirname(suiteFile);
    if (!suites || suites.has(suiteName) || suites.has(suiteFile) || suites.has(pathToDir)) {
      const {default: suite} = (require: any)(suiteFile);
      if (!(suite instanceof Suite)) {
        throw new Error(format(
          "Test suite `%s` forgot to export default suite(...)",
          suiteFile,
        ));
      }
      result[suiteName] = suite;
    }
  }

  return result;
}
