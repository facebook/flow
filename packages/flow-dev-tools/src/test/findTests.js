/**
 * @flow
 * @format
 */

import {dirname, normalize, relative, resolve} from 'path';
import {format} from 'util';

import {exists, glob, readFile} from '../utils/async';
import Builder from './builder';
import {getTestsDir} from '../constants';
import Suite from './Suite';

import type {Tests} from './Tester';
import type {SuiteResult} from './runTestSuite';

const testSuiteRegex = /(.*)[\/\\]test.js/;

async function findTestSuites(): Promise<Array<string>> {
  const testSuites = await glob(format('%s/**/test.js', getTestsDir()), {
    cwd: __dirname,
  });
  // On Windows, glob still uses unix dir seperators, so we need to normalize
  return testSuites.map(normalize);
}

export async function findTestsByName(
  suitesOrig: ?Set<string>,
): Promise<Set<string>> {
  let suites = null;
  if (suitesOrig != null) {
    suites = new Set();
    for (let suite of suitesOrig) {
      suite = normalize(suite.trim());
      suites.add(suite);
      suites.add(resolve(suite));
    }
  }
  const testsDir = getTestsDir();
  const testSuites = await findTestSuites();

  const result = new Set();

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
  const {default: suite} = (require: any)(filename);
  // I don't know why, but suite instanceof Suite doesn't seem to work when
  // using symlinks. So this is a fuzzy approximation
  if (!(suite && suite.constructor && suite.constructor.name === Suite.name)) {
    throw new Error(
      format('Test suite `%s` forgot to export default suite(...)', filename),
    );
  }
  return suite;
}

export function loadSuite(suiteName: string): Suite {
  return loadSuiteByFilename(resolve(getTestsDir(), suiteName, 'test.js'));
}

export async function findTestsByRun(
  runID: string,
  failedOnly: boolean,
): Promise<Set<string>> {
  const runDir = Builder.getDirForRun(runID);
  const runDirExists = await exists(runDir);

  if (!runID || !runDirExists) {
    process.stderr.write(
      format("Cannot find the tmp directory for run '%s'.\n", runID),
    );
    return new Set();
  }

  const results = await glob(format('%s/*/results.json', runDir), {
    cwd: __dirname,
  });

  let resultContents = await Promise.all(
    results.map(async filename => {
      const rawContents = await readFile(filename);
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

  return findTestsByName(suites);
}
