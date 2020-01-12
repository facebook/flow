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

export type Args = {
  dest: string,
  src: string,
  transform: 'all' | 'tool' | 'tests',
};

export default class BabelCommand extends Base<Args> {
  static processArgv(argv: Object): Args {
    return {
      dest: argv.dest || resolve('bin'),
      src: argv.src || resolve('.'),
      transform: argv.transform,
    };
  }

  static async run(args: Args): Promise<void> {
    require('./babelRunner').default(args);
  }

  static description(): string {
    return "Runs babel";
  }

  static async usage(): Promise<string> {
    return `usage: ${process.argv[1]} babel [OPTION]...
`;
  }

  static getFlags() {
    return [
      {
        type: "string",
        name: "dest",
        argName: "pathToDest",
        description: "Where the transformed code should end up",
      },
      {
        type: "string",
        name: "src",
        argName: "pathToSrc",
        description:
          "The root (containing the package.json) of the code to transform",
      },
      {
        type: "enum",
        name: "transform",
        argName: "string",
        description: "What to transform",
        validValues: ["all", "tool", "tests"],
        default: "all",
      },
    ];
  }
}
