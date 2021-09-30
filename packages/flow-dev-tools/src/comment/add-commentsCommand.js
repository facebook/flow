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

const {format} = require('util');
const {resolve} = require('path');

const {commonFlags, default: Base} = require('../command/Base');
const findFlowBin = require('../command/findFlowBin').default;

export type Args = {
  all: boolean,
  bin: string,
  flowconfigName: string,
  comment: ?string,
  error_code: ?string,
  errorCheckCommand: 'check' | 'status',
  root: string,
};

class AddCommentsCommand extends Base<Args> {
  static processArgv(argv: Object): Args {
    if (argv._.length !== 1) {
      this.showUsage(this.BAD_ARGS);
    }
    return {
      all: argv.all,
      bin: findFlowBin(argv.bin),
      flowconfigName: argv.flowconfigName,
      errorCheckCommand: argv.check,
      root: resolve(process.cwd(), argv._[0]),
      comment: argv.comment,
      error_code: argv.code,
    };
  }

  static async run(args: Args): Promise<void> {
    require('./add-commentsRunner').default(args);
  }

  static description(): string {
    return 'Adds flow comments';
  }

  static async usage(): Promise<string> {
    return `usage: ${process.argv[1]} add-comments [OPTION]... ROOT

Queries Flow for the errors for ROOT. Then opens a curses interface to let you select errors. The selected errors will automatically have a comment added on the line before them.
`;
  }

  static getFlags(): Array<Flag> {
    return [
      commonFlags.bin,
      commonFlags.flowconfigName,
      commonFlags.errorCheckCommand,
      {
        type: 'string',
        name: 'comment',
        argName: '"foo"',
        description:
          'Comment to add before the selected errors. If not set, you will be prompted later for it',
      },
      {
        type: 'boolean',
        name: 'all',
        description: 'Select all errors',
      },
      {
        type: 'string',
        name: 'code',
        argName: '"code"',
        description: 'Only add comments for a specific code',
      },
    ];
  }
}

module.exports = {default: AddCommentsCommand};
