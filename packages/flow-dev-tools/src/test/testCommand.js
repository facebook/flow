/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import type {Flag} from '../command/Base';

import {resolve} from 'path';
import {format} from 'util';

import {defaultTestsDirName} from '../constants';
import Base, {commonFlags} from '../command/Base';
import findFlowBin from '../command/findFlowBin';

export type Args = {
  suites: ?Set<string>,
  bin: string,
  fbmakeJson: boolean,
  parallelism: number,
  errorCheckCommand: 'check' | 'status',
  rerun: ?string,
  failedOnly: boolean,
  watch: boolean,
  buckCpTestsDir: ?string,
  maxErroredTests: ?number,
  maxErroredTestsPct: ?number,
};

export default class TestCommand extends Base<Args> {
  static processArgv(argv: Object): Args {
    const suites = argv._.length > 0 ? new Set(argv._) : null;
    if (argv.rerun != null && argv['rerun-failed'] != null) {
      process.stderr.write('You cannot set both --rerun and --rerun-failed\n');
      this.showUsage(this.BAD_ARGS);
    }
    if (
      argv['max-errored-tests'] != null &&
      argv['max-errored-tests-pct'] != null
    ) {
      process.stderr.write(
        'You cannot set both --max-errored-tests and --max-errored-tests-pct.' +
          ' Use one or the other.\n',
      );
      this.showUsage(this.BAD_ARGS);
    }
    return {
      suites,
      bin: findFlowBin(argv.bin),
      fbmakeJson: argv.fbmakeJson,
      parallelism: argv.parallelism,
      errorCheckCommand: argv.check,
      rerun: argv.rerun || argv['rerun-failed'],
      failedOnly: !!argv['rerun-failed'],
      watch: Boolean(argv.watch),
      buckCpTestsDir: argv['buck-copy-tests-dir'],
      maxErroredTests: argv['max-errored-tests'],
      maxErroredTestsPct: argv['max-errored-tests-pct'],
    };
  }

  static async run(args: Args): Promise<void> {
    await require('./testRunner').default(args);
  }

  static description(): string {
    return 'Runs tests';
  }

  static async usage(): Promise<string> {
    return `usage: ${process.argv[1]} test [OPTION]... [SUITE]...

SUITE
    Given the test \`path/to/testdir/mytest/test.js\`, the following values of
    SUITE will run that test:
        * mytest
        * path/to/testdir/mytest
        * path/to/testdir/mytest/test.js

    If no suites are specified, every test in the test directory will be run.`;
  }

  static getFlags(): Array<Flag> {
    return [
      commonFlags.bin,
      commonFlags.parallelism,
      commonFlags.errorCheckCommand,
      {
        type: 'boolean',
        name: 'fbmakeJson',
        description: 'Output JSON for fbmake',
      },
      {
        type: 'string',
        name: 'rerun',
        argName: 'RUN',
        description: 'Rerun tests from a previous test run',
      },
      {
        type: 'string',
        name: 'rerun-failed',
        argName: 'RUN',
        description: 'Rerun failed tests from a previous test run',
      },
      {
        type: 'boolean',
        name: 'watch',
        description: 'Automatically rerun tests when they change',
      },
      {
        type: 'string',
        name: 'buck-copy-tests-dir',
        argName: 'PATH',
        description: format(
          "You probably don't want to use this option. " +
            "It's basically `rm %s; cp $PATH %s`.",
          defaultTestsDirName,
          defaultTestsDirName,
        ),
      },
      {
        type: 'string',
        name: 'max-errored-tests',
        argName: 'N',
        description:
          'If more than N tests error, test run will return a non-zero exit code',
      },
      {
        type: 'string',
        name: 'max-errored-tests-pct',
        argName: 'P',
        description:
          'If more than P% tests error, test run will return a non-zero exit code',
      },
    ];
  }
}
