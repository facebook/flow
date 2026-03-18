/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {resolve} = require('path');

const Base = require('../command/Base');
const {commonFlags} = Base;
const findFlowBin = require('../command/findFlowBin').default;
const {getCheckTestsDir} = require('../constants');
const {checkTestRunner} = require('./checkTestRunner');

import type {Flag} from '../command/Base';

type Args = {
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

class CheckTestCommand extends Base.default<Args> {
  static description(): string {
    return "Runs Flow's check tests (tests/ directory)";
  }

  static async usage(): Promise<string> {
    return `usage: tool check-test [OPTIONS] [TEST_FILTER]

Runs Flow's bash-style tests from the tests/ directory using a
cross-platform Node.js runner. This replaces runtests.sh.`;
  }

  static getFlags(): Array<Flag> {
    return [
      commonFlags.bin,
      {
        type: 'string',
        name: 'tests-dir',
        argName: 'path/to/tests',
        description: 'Path to tests directory',
        aliases: ['d'],
      },
      {
        type: 'string',
        name: 'filter',
        argName: 'REGEX',
        description: 'Regular expression to filter test names',
        aliases: ['f'],
      },
      {
        type: 'string',
        name: 'test',
        argName: 'TEST_NAME',
        description: 'Run a specific test (equivalent to filter "^TEST_NAME$")',
        aliases: ['t'],
      },
      commonFlags.parallelism,
      {
        type: 'boolean',
        name: 'check-only',
        description: 'Only run full-check tests',
        aliases: ['c'],
      },
      {
        type: 'boolean',
        name: 'saved-state',
        description: 'Test using saved state',
        aliases: ['s'],
      },
      {
        type: 'boolean',
        name: 'long-lived-workers',
        description: 'Test with long-lived workers',
        aliases: ['L'],
      },
      {
        type: 'boolean',
        name: 'record',
        description: 'Re-record failing tests to update expected output',
        aliases: ['r'],
      },
      {
        type: 'boolean',
        name: 'quiet',
        description: 'Quiet output (hides status, just prints results)',
        aliases: ['q'],
      },
      {
        type: 'boolean',
        name: 'verbose',
        description: 'Verbose output (shows skipped tests)',
        aliases: ['v'],
      },
      {
        type: 'boolean',
        name: 'json',
        description: 'Output results as a JSON map',
        aliases: ['j'],
      },
      {
        type: 'boolean',
        name: 'list',
        description: 'List tests that will be run',
        aliases: ['l'],
      },
    ];
  }

  static processArgv(argv: Object): Args {
    const bin = findFlowBin(argv.bin);

    // Determine tests directory
    const testsDir = getCheckTestsDir(argv['tests-dir'] || null);

    // Handle specific test (-t) as filter
    let filter = argv.filter || null;
    if (argv.test) {
      let testName = argv.test;
      // Strip suffixes like -saved-state or -long-lived-workers
      if (argv['saved-state']) {
        testName = testName.replace(/-saved-state$/, '');
      }
      if (argv['long-lived-workers']) {
        testName = testName.replace(/-long-lived-workers$/, '');
      }
      filter = '^' + testName + '$';
    }

    // Positional arg as filter fallback
    if (!filter && argv._.length > 0) {
      filter = argv._[0];
    }

    const jsonOutput = Boolean(argv.json);

    return {
      bin,
      testsDir,
      filter,
      parallelism: parseInt(argv.parallelism, 10) || 16,
      checkOnly: Boolean(argv['check-only']),
      savedState: Boolean(argv['saved-state']),
      longLivedWorkers: Boolean(argv['long-lived-workers']),
      record: Boolean(argv.record),
      quiet: Boolean(argv.quiet) || jsonOutput,
      verbose: Boolean(argv.verbose),
      jsonOutput,
      listTests: Boolean(argv.list),
    };
  }

  static async run(args: Args): Promise<void> {
    await checkTestRunner(args);
  }
}

module.exports = {default: CheckTestCommand};
