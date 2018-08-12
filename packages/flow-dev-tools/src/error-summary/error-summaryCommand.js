/* @flow
 * @format
 */

import {format} from 'util';
import {resolve} from 'path';

import Base, {commonFlags} from '../command/Base';
import findFlowBin from '../command/findFlowBin';

export type Args = {
  bin: string,
  errorCheckCommand: 'check' | 'status',
  root: string,
  messageFilter: ?string,
  codeFilter: ?string,
  fileFilter: ?string,
  showErrors: ?boolean,
  showFiles: ?boolean,
};

export default class ErrorSummaryCommand extends Base<Args> {
  static processArgv(argv: Object): Args {
    if (argv._.length !== 1) {
      this.showUsage(this.BAD_ARGS);
    }
    return {
      bin: findFlowBin(argv.bin),
      errorCheckCommand: argv.check,
      root: resolve(process.cwd(), argv._[0]),
      messageFilter: argv.messageFilter,
      codeFilter: argv.codeFilter,
      fileFilter: argv.fileFilter,
      showErrors: argv.showErrors,
      showFiles: argv.showFiles,
    };
  }

  static async run(args: Args): Promise<void> {
    require('./error-summaryRunner').default(args);
  }

  static description(): string {
    return 'Shows a list of how many times each error message occurs in a flow check';
  }

  static async usage(): Promise<string> {
    return `usage: ${process.argv[1]} error-summary [OPTION]... ROOT

Queries Flow for the errors for ROOT. Then logs how many times each error message occured, in descending order.
`;
  }

  static getFlags() {
    return [
      commonFlags.bin,
      commonFlags.errorCheckCommand,
      {
        type: 'string',
        name: 'messageFilter',
        argName: '<regex>',
        description: 'Regex used to filter by error messages into the summary',
      },
      {
        type: 'string',
        name: 'codeFilter',
        argName: '<regex>',
        description:
          'Regex used to filter by error blame code into the summary',
      },
      {
        type: 'string',
        name: 'fileFilter',
        argName: '<regex>',
        description: 'Regex used to filter by file name into the summary',
      },
      {
        type: 'boolean',
        name: 'showErrors',
        description: 'If set to true, error messages will print',
      },
      {
        type: 'boolean',
        name: 'showFiles',
        description: 'If set to true, file names will print',
      },
    ];
  }
}
