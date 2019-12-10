/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import {format} from 'util';

import Base, {commonFlags} from './../command/Base';
import findFlowBin from './../command/findFlowBin';

export type Args = {
  dirs: Set<string>,
  bin: string,
  sanity: boolean,
  parallelism: number,
  errorCheckCommand: "status" | "check",
};

export default class ConvertCommand extends Base<Args> {
  static processArgv(argv: Object): Args {
    return {
      dirs: new Set(argv._),
      bin: findFlowBin(argv.bin),
      sanity: argv.sanity,
      parallelism: argv.parallelism,
      errorCheckCommand: argv.check,
    };
  }

  static async run(args: Args): Promise<void> {
    if (args.dirs.size === 0) {
      process.stderr.write("Error: No legacy test directories specified\n");
      return this.showUsage(this.BAD_ARGS);
    }
    require('./convertRunner').default(args);
  }

  static description(): string {
    return "Converts legacy tests to new j0nx";
  }

  static async usage(): Promise<string> {
    return `usage: ${process.argv[1]} convert [OPTION]... LEGACY_TEST_DIR...

LEGACY_TEST_DIR
    This is the path to the legacy test directory. Given
    \`path/to/legacy/tests/mytest\`, convert will create
    \`path/to/new/tests/legacy/mytest\`. At least one
    legacy test must be provided.
`;
  }

  static getFlags() {
    return [
      commonFlags.bin,
      commonFlags.parallelism,
      commonFlags.errorCheckCommand,
      {
        type: "boolean",
        name: "sanity",
        description: "Don't convert, just run sanity check",
      },
    ];
  }
}
