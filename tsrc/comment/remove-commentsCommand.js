/* @flow */

import {format} from 'util';
import {resolve} from 'path';

import Base, {commonFlags} from '../command/Base';
import findFlowBin from '../command/findFlowBin';

export type Args = {
  bin: string,
  errorCheckCommand: 'check' | 'status',
  root: string,
};

export default class RemoveCommentsCommand extends Base<Args> {
  static processArgv(argv: Object): Args {
    if (argv._.length !== 1) {
      this.showUsage(this.BAD_ARGS);
    }
    return {
      bin: findFlowBin(argv.bin),
      errorCheckCommand: argv.check,
      root: resolve(process.cwd(), argv._[0]),
    };
  }

  static async run(args: Args): Promise<void> {
    require('./remove-commentsRunner').default(args);
  }

  static description(): string {
    return "Removes unused flow comments";
  }

  static async usage(): Promise<string> {
return `usage: ${process.argv[1]} remove-comments [OPTION]... ROOT

Queries Flow for the unused error suppressions for ROOT. Then removes them from the code.
`;
  }

  static getFlags() {
    return [
      commonFlags.bin,
      commonFlags.errorCheckCommand,
    ];
  }
}
