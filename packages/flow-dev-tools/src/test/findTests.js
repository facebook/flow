/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {access, readFile} = require('fs').promises;
const {dirname, normalize, relative, resolve} = require('path');
const {format} = require('util');

const {glob} = require('../utils/async');
const {default: Builder} = require('./builder');
const {getTestsDir} = require('../constants');

import type {Suite} from './Suite';
import type {Tests} from './Tester';
import type {SuiteResult} from './runTestSuite';

const testSuiteRegex = /(.*)[\/\\]test.js/;

async function findTestSuites(testsDir: string): Promise<Array<string>> {
  const testSuites = await glob(format('%s/**/test.js', testsDir), {
    cwd: __dirname,
  });
  // On Windows, glob still uses unix dir separators, so we need to normalize
  return testSuites.map(normalize);
}

async function findTestsByName(
  providedTestsDir: ?string,
  suitesOrig: ?Set<string>,
): Promise<Set<string>> {
  let suites = null;
  if (suitesOrig != null) {
    suites = new Set<string>();
    for (let suite of suitesOrig) {
      suite = normalize(suite.trim());
      suites.add(suite);
      suites.add(resolve(suite));
    }
  }
  const testsDir = getTestsDir(providedTestsDir);
  const testSuites = await findTestSuites(testsDir);

  const result = new Set<string>();

  process.stderr.write(format('Found %d suites\n', testSuites.length));
  for (const suiteFile of testSuites) {
    const suiteName = relative(testsDir, suiteFile).replace(
      testSuiteRegex,
      '$1',
    );
    const pathToDir = dirname(suiteFile);
    if (
      !suites ||
      suites.has(suiteName) ||
      suites.has(suiteFile) ||
      suites.has(pathToDir)
    ) {
      result.add(suiteName);
    }
  }

  return result;
}

function loadSuiteByFilename(filename: string): Suite {
  delete require.cache[require.resolve(filename)];
  const suite = (require: any)(filename);
  // I don't know why, but suite instanceof Suite doesn't seem to work when
  // using symlinks. So this is a fuzzy approximation
  if (!(suite && suite.__SUITE__)) {
    throw new Error(
      format('Test suite `%s` forgot to export default suite(...)', filename),
    );
  }
  return suite;
}

function loadSuite(testsDir: ?string, suiteName: string): Suite {
  return loadSuiteByFilename(
    resolve(getTestsDir(testsDir), suiteName, 'test.js'),
  );
}

async function exists(dir: string): Promise<boolean> {
  try {
    await access(dir);
    return true;
  } catch (_) {
    return false;
  }
}

async function findTestsByRun(
  runID: string,
  failedOnly: boolean,
  testsDir: ?string,
): Promise<Set<string>> {
  const runDir = Builder.getDirForRun(runID);
  const runDirExists = await exists(runDir);

  if (!runID || !runDirExists) {
    process.stderr.write(
      format("Cannot find the tmp directory for run '%s'.\n", runID),
    );
    return new Set();
  }

  const results = await glob(format('%s/**/results.json', runDir), {
    cwd: __dirname,
  });

  let resultContents = await Promise.all(
    results.map(async filename => {
      const rawContents = await readFile(filename, 'utf8');
      try {
        return JSON.parse(rawContents);
      } catch (e) {
        throw new Error(
          format('Error parsing run %s result %s:\n%s', runID, filename, e),
        );
      }
    }),
  );

  if (failedOnly) {
    resultContents = resultContents.filter(contents => {
      const suiteResult: SuiteResult = contents.results;
      if (suiteResult.type === 'exceptional') {
        return true;
      } else if (suiteResult.type === 'normal') {
        return suiteResult.testResults.some(testResult =>
          testResult.stepResults.some(stepResult => !stepResult.passed),
        );
      }
    });
  }

  const suites = new Set(resultContents.map(contents => contents.suiteName));

  process.stderr.write(
    format(
      'Looks like run %s ran %d suites%s\n',
      runID,
      suites.size,
      failedOnly ? ' that failed' : '',
    ),
  );

  return findTestsByName(testsDir, suites);
}

module.exports = {
  findTestsByName,
  loadSuite,
  findTestsByRun,
};
