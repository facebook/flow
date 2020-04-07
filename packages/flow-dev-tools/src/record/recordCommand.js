/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import {format} from 'util';

import Base, {commonFlags} from '../command/Base';
import findFlowBin from '../command/findFlowBin';

export type Args = {
  suites: ?Set<string>,
  bin: string,
  parallelism: number,
  errorCheckCommand: 'check' | 'status',
  rerun: ?string,
};

export default class RecordCommand extends Base<Args> {
  static processArgv(argv: Object): Args {
    const suites = argv._.length > 0 ? new Set(argv._) : null;
    return {
      suites,
      bin: findFlowBin(argv.bin),
      parallelism: argv.parallelism,
      errorCheckCommand: argv.check,
      rerun: argv['rerun-failed'],
    };
  }

  static async run(args: Args): Promise<void> {
    require('./recordRunner').default(args);
  }

  static description(): string {
    return 'Records tests';
  }

  static async usage(): Promise<string> {
    return `usage: ${process.argv[1]} record [OPTION]... [SUITE]...

SUITE
    Given the test \`path/to/testdir/mytest/test.js\`, the following values of
    SUITE will record that test:
        * mytest
        * path/to/testdir/mytest
        * path/to/testdir/mytest/test.js

    If no suites are specified, every test in the test directory will be recorded.`;
  }

  static getFlags() {
    return [
      commonFlags.bin,
      commonFlags.parallelism,
      commonFlags.errorCheckCommand,
      {
        type: 'string',
        name: 'rerun-failed',
        argName: 'RUN',
        description: 'Record failed tests from a previous test run',
      },
    ];
  }
}
