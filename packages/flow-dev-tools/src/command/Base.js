/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import {format} from 'util';

import parseArgs from 'minimist';

export type Flag =
  | $Exact<{
      type: 'string',
      name: string,
      description: string,
      argName: string,
      aliases?: Array<string>,
      default?: string,
    }>
  | $Exact<{
      type: 'boolean',
      name: string,
      description: string,
      aliases?: Array<string>,
    }>
  | $Exact<{
      type: 'enum',
      name: string,
      description: string,
      argName: string,
      validValues: Array<string>,
      aliases?: Array<string>,
      default?: string,
    }>;

export const commonFlags = {
  bin: {
    type: 'string',
    name: 'bin',
    argName: 'path/to/flow',
    description: 'Path to the flow binary',
  },
  flowconfigName: {
    type: 'string',
    name: 'flowconfigName',
    argName: '.flowconfig',
    description: 'Name of the flowconfig to use in checking',
    default: '.flowconfig',
  },
  parallelism: {
    type: 'string',
    name: 'parallelism',
    argName: 'N',
    description: 'Number of tests to run in parallel',
    aliases: ['p'],
    default: '16',
  },
  errorCheckCommand: {
    type: 'enum',
    name: 'check',
    argName: 'COMMAND',
    description: 'The flow command to check flow errors',
    validValues: ['check', 'status'],
    aliases: ['c'],
    default: 'status',
  },
};

class ShowUsageException {
  exitCode: number;

  constructor(exitCode: number) {
    this.exitCode = exitCode;
  }
}

export default class Base<T: Object> {
  static BAD_ARGS = 64;
  static OK = 0;

  // abstract
  static processArgv(argv: Object): T {
    throw new Error('Unimplemented abstract method: parse');
  }

  // abstract
  static async run(args: T): Promise<void> {
    throw new Error('Unimplemented abstract method: run');
  }

  static description(): string {
    throw new Error('Unimplemented abstract method: description');
  }

  // abstract
  static async usage(): Promise<string> {
    throw new Error('Unimplemented abstract method: usage');
  }

  // override
  static getFlags(): Array<Flag> {
    return [];
  }

  static getAllFlags(): Array<Flag> {
    return [].concat(this.getFlags(), {
      type: 'boolean',
      name: 'help',
      description: 'Shows this usage message',
      aliases: ['h'],
    });
  }

  // final
  static parse() {
    const boolean = [];
    const string = [];
    const defaults = {};
    const alias = {};

    const flags = this.getAllFlags();
    for (const flag of flags) {
      if (flag.type === 'string' || flag.type === 'enum') {
        string.push(flag.name);
        flag.default !== undefined && (defaults[flag.name] = flag.default);
      } else {
        boolean.push(flag.name);
      }
      if (flag.aliases !== undefined) {
        for (const aliasName of flag.aliases) {
          alias[aliasName] = flag.name;
        }
      }
    }

    const argv = parseArgs(process.argv.slice(3), {
      boolean,
      string,
      alias,
      default: defaults,
      unknown: flag => {
        if (flag.match(/^-/)) {
          process.stderr.write(format('Unsupported flag:', flag, '\n'));
          this.showUsage(this.BAD_ARGS);
        }
      },
    });
    for (const flag of flags) {
      if (flag.type === 'string' || flag.type === 'enum') {
        if (argv[flag.name] === '') {
          process.stderr.write(
            format('Missing required argument for flag: %s\n', flag.name),
          ),
            this.showUsage(this.BAD_ARGS);
        }
      }
      if (flag.type === 'enum') {
        if (flag.validValues.find(v => argv[flag.name] === v) === undefined) {
          process.stderr.write(
            format(
              '%s is not a support value for enum flag %s\n',
              argv[flag.name],
              flag.name,
            ),
          );
          this.showUsage(this.BAD_ARGS);
        }
      }
    }
    return argv;
  }

  // final
  static showUsage(exitCode: number) {
    throw new ShowUsageException(exitCode);
  }

  static async displayUsage(exn: ShowUsageException) {
    let usage = await this.usage();

    const flags = this.getAllFlags().sort((a, b) =>
      a.name.localeCompare(b.name),
    );

    usage += '\n\nOPTIONS:';
    for (const flag of flags) {
      const arg =
        flag.type === 'string' || flag.type === 'enum'
          ? ' ' + flag.argName
          : '';
      const values =
        flag.type === 'enum'
          ? format(
              ' [%s]',
              flag.validValues.map(x => format('"%s"', x)).join(', '),
            )
          : '';
      const defaultDesc =
        flag.default !== undefined
          ? format(' (default: "%s")', flag.default)
          : '';
      const names = []
        .concat(flag.name, flag.aliases || [])
        .map(name => (name.length == 1 ? '-' : '--') + name + arg)
        .join(', ');
      usage += '\n    ' + names;
      usage += '\n        ' + flag.description + values + defaultDesc;
    }

    usage += '\n';

    process.stderr.write(usage);
    process.exit(exn.exitCode);
  }

  // final
  static async go() {
    try {
      const argv = this.parse();
      if (argv.help) {
        this.showUsage(this.OK);
      } else {
        await this.run(this.processArgv(argv));
      }
    } catch (e) {
      if (e instanceof ShowUsageException) {
        await this.displayUsage(e);
      } else {
        throw e;
      }
    }
  }
}
