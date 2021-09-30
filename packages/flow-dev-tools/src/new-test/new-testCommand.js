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

const {commonFlags, default: Base} = require('../command/Base');
const findFlowBin = require('../command/findFlowBin').default;

export type Args = {
  names: Set<string>,
  bin: string,
};

class NewTestCommand extends Base<Args> {
  static processArgv(argv: Object): Args {
    return {
      names: new Set(argv._),
      bin: findFlowBin(argv.bin),
    };
  }

  static async run(args: Args): Promise<void> {
    require('./new-testRunner').default(args);
  }

  static description(): string {
    return 'Creates a new empty test';
  }

  static async usage(): Promise<string> {
    return `usage: ${process.argv[1]} new-test [OPTION]... NAME...

NAME
    If NAME is foo, then \`path/to/testdir/foo\` will be created with the
    various files that a new test needs.
`;
  }

  static getFlags(): Array<Flag> {
    return [commonFlags.bin];
  }
}

module.exports = {
  default: NewTestCommand,
};
