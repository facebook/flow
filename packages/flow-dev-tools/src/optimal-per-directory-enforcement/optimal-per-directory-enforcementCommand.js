/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import type {Flag} from '../command/Base';

const {format} = require('util');
const {resolve} = require('path');

const {commonFlags, default: Base} = require('../command/Base');
const findFlowBin = require('../command/findFlowBin').default;

export type Args = {
  bin: string,
  flowconfigName: string,
  errorCheckCommand: 'check' | 'status',
  root: string,
  maxDepth: number,
};

class OptimalPerDirectoryEnforcementCommand extends Base<Args> {
  static processArgv(argv: Object): Args {
    if (argv._.length !== 1) {
      this.showUsage(this.BAD_ARGS);
    }
    return {
      bin: findFlowBin(argv.bin),
      flowconfigName: argv.flowconfigName,
      errorCheckCommand: argv.check,
      root: resolve(process.cwd(), argv._[0]),
      maxDepth: parseInt(argv['max-depth'], 10),
    };
  }

  static async run(args: Args): Promise<void> {
    require('./optimal-per-directory-enforcementRunner').default(args);
  }

  static description(): string {
    return 'Shows a list of how many times each error message occurs in a flow check';
  }

  static async usage(): Promise<string> {
    return `usage: ${process.argv[1]} optimal-per-directory-enforcement [OPTION]... ROOT

Queries Flow for the errors for ROOT and a list of files covered by Flow. Then computes a smallest list of directories to turn on an enforcement flag.
`;
  }

  static getFlags(): Array<Flag> {
    return [
      commonFlags.bin,
      commonFlags.flowconfigName,
      commonFlags.errorCheckCommand,
      {
        type: 'string',
        name: 'max-depth',
        argName: 'N',
        description: 'Maximum allowed path depth',
        default: '1000',
      },
    ];
  }
}

module.exports = {
  default: OptimalPerDirectoryEnforcementCommand,
};
