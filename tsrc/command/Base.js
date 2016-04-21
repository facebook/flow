/* @flow */

import {format} from 'util';

import parseArgs from 'minimist';

export type Flag = {
  type: "string",
  name: string,
  description: string,
  argName: string,
  aliases?: Array<string>,
  default?: string,
} | {
  type: "boolean",
  name: string,
  description: string,
  aliases?: Array<string>,
};

export const commonFlags = {
  bin: {
    type: "string",
    name: "bin",
    argName: "path/to/flow",
    description: "Path to the flow binary",
  },
  parallelism: {
    type: "string",
    name: "parallelism",
    argName: "N",
    description: "Number of tests to run in parallel",
    aliases: ["p"],
    default: "16",
  },
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
    return [].concat(
      this.getFlags(),
      {
        type: "boolean",
        name: "help",
        description: "Shows this usage message",
        aliases: ["h"],
      },
    );
  }

  // final
  static parse() {
    const boolean = [];
    const string = [];
    const defaults = {};
    const alias = {};

    for (const flag of this.getAllFlags()) {
      if (flag.type === "string") {
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

    return parseArgs(
      process.argv.slice(3),
      {
        boolean,
        string,
        alias,
        default: defaults,
        unknown: flag => {
          if (flag.match(/^-/)) {
            process.stderr.write(format('Unsupported flag:', flag, "\n"));
            this.showUsage(this.BAD_ARGS);
          }
        }
      }
    );
  }

  // final
  static async showUsage(exitCode) {
    let usage = await this.usage();

    const flags = this.getAllFlags()
      .sort((a, b) => a.name.localeCompare(b.name));

    usage += "\n\nOPTIONS:";
    for (const flag of flags) {
      const arg = flag.type === "string" ? " "+flag.argName : "";
      const defaultDesc = flag.type === "string" && flag.default !== undefined
        ? format(' (default: "%s")', flag.default)
        : "";
      const names = []
        .concat(flag.name, flag.aliases || [])
        .map(name => (name.length == 1 ? '-' : '--') + name + arg)
        .join(", ");
      usage += "\n    " + names
      usage += "\n        " + flag.description + defaultDesc;
    }

    usage += "\n";

    process.stderr.write(usage);
    process.exit(exitCode);
  }

  // final
  static async go() {
    const argv = this.parse();
    if (argv.help) {
      await this.showUsage(this.OK);
    } else {
      await this.run(this.processArgv(argv));
    }
  }
}
