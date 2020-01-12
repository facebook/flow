/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import {format} from 'util';
import {resolve} from 'path';

import Base, {commonFlags} from '../command/Base';
import findFlowBin from '../command/findFlowBin';

export type Args = {
  all: boolean,
  bin: string,
  flowconfigName: string,
  comment: ?string,
  errorCheckCommand: 'check' | 'status',
  root: string,
};

export default class AddCommentsCommand extends Base<Args> {
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
    };
  }

  static async run(args: Args): Promise<void> {
    require('./add-commentsRunner').default(args);
  }

  static description(): string {
    return "Adds flow comments";
  }

  static async usage(): Promise<string> {
return `usage: ${process.argv[1]} add-comments [OPTION]... ROOT

Queries Flow for the errors for ROOT. Then opens a curses interface to let you select errors. The selected errors will automatically have a comment added on the line before them.
`;
  }

  static getFlags() {
    return [
      commonFlags.bin,
      commonFlags.flowconfigName,
      commonFlags.errorCheckCommand,
      {
        type: "string",
        name: "comment",
        argName: '"\\$FlowFixMe foo"',
        description: "Comment to add before the selected errors. If not set, you will be prompted later for it",
      },
      {
        type: "boolean",
        name: "all",
        description: "Select all errors",
      }
    ];
  }
}
