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

import {format} from 'util';
import {resolve} from 'path';

import Base, {commonFlags} from '../command/Base';
import findFlowBin from '../command/findFlowBin';

export type Args = {
  bin: string,
  flowconfigName: string,
  errorCheckCommand: 'check' | 'status',
  root: string,
  includeFlowtest: boolean,
};

export default class RemoveCommentsCommand extends Base<Args> {
  static processArgv(argv: Object): Args {
    if (argv._.length !== 1) {
      this.showUsage(this.BAD_ARGS);
    }
    return {
      bin: findFlowBin(argv.bin),
      flowconfigName: argv.flowconfigName,
      errorCheckCommand: argv.check,
      root: resolve(process.cwd(), argv._[0]),
      includeFlowtest: !!argv['include-flowtest'],
    };
  }

  static async run(args: Args): Promise<void> {
    require('./remove-commentsRunner').default(args);
  }

  static description(): string {
    return 'Removes unused flow comments';
  }

  static async usage(): Promise<string> {
    return `usage: ${process.argv[1]} remove-comments [OPTION]... ROOT

Queries Flow for the unused error suppressions for ROOT. Then removes them from the code.
`;
  }

  static getFlags(): Array<Flag> {
    return [
      commonFlags.bin,
      commonFlags.flowconfigName,
      commonFlags.errorCheckCommand,
      {
        type: 'boolean',
        name: 'include-flowtest',
        description:
          'Also remove comments from files that end in ' +
          '-flowtest.js or are in a __flowtests__ directory',
      },
    ];
  }
}
