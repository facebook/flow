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

import {realpathSync} from 'fs';
import {format} from 'util';
import {resolve} from 'path';

import Base, {commonFlags} from '../command/Base';
import findFlowBin from '../command/findFlowBin';

export opaque type RootPath: string = string;
export opaque type RootName: string = string;

export type Args = {
  bin: string,
  diffBin: ?string,
  flowconfigName: string,
  errorCheckCommand: 'check' | 'status',
  comment: string,
  roots: Array<RootPath>,
  rootNames: Array<RootName>,
  includeFlowtest: boolean,
  only: ?('add' | 'remove'),
};

export default class UpdateSuppressionsCommand extends Base<Args> {
  static processArgv(argv: Object): Args {
    if (argv._.length < 1) {
      this.showUsage(this.BAD_ARGS);
    }

    return {
      bin: findFlowBin(argv.bin),
      diffBin: argv['diffBin'] ? findFlowBin(argv['diffBin']) : null,
      flowconfigName: argv.flowconfigName,
      errorCheckCommand: argv.check,
      roots: argv._.map(root => realpathSync(resolve(process.cwd(), root))),
      rootNames: (argv.sites || '': RootName)
        .split(',')
        .map(site => site.trim()),
      includeFlowtest: !!argv['include-flowtest'],
      comment: argv['comment'] ? argv['comment'] : '',
      only: argv['only'],
    };
  }

  static async run(args: Args): Promise<void> {
    require('./update-suppressionsRunner').default(args);
  }

  static description(): string {
    return 'Adds and removes suppression comments';
  }

  static async usage(): Promise<string> {
    return `usage: ${process.argv[1]} update-suppressions [OPTION]... ROOT [ROOT...]

Removes unnecessary, and adds necessary, error suppression comments for ROOT.
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
      {
        type: 'string',
        name: 'sites',
        argName: 'SITE[,SITE[,...]]',
        description: 'Comma-delimited list of site names for each ROOT',
      },
      {
        type: 'string',
        name: 'comment',
        argName: '<COMMENT>',
        description:
          'Comment to include with the suppression. Automatically prepends $FlowFixMe',
      },
      {
        type: 'string',
        name: 'only',
        argName: '"add" | "remove"',
        description:
          'Use --only add to only add comments and --only remove to only remove comments',
      },
      {
        type: 'string',
        name: 'diffBin',
        argName: 'path/to/bin',
        description:
          'Path to another binary to use. If specified, update-suppressions will remove suppressions if they are used in both of the two binaries, and suppress errors in either',
      },
    ];
  }
}
