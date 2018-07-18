/* @flow */

import {format} from 'util';

import Base, {commonFlags} from '../command/Base';
import findFlowBin from '../command/findFlowBin';

export type Args = {
  names: Set<string>,
  bin: string,
};

export default class NewTestCommand extends Base<Args> {
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
    return "Creates a new empty test";
  }

  static async usage(): Promise<string> {
    return `usage: ${process.argv[1]} new-test [OPTION]... NAME...

NAME
    If NAME is foo, then \`path/to/testdir/foo\` will be created with the
    various files that a new test needs.
`;
  }

  static getFlags() {
    return [
      commonFlags.bin,
    ];
  }
}
