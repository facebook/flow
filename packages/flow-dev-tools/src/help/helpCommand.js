/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {format} = require('util');

const {default: Base} = require('../command/Base');
const commandFinder = require('../command/finder');

type Args = {
  command: ?string,
};

class HelpCommand extends Base<Args> {
  static processArgv(argv: Object): Args {
    const command = argv._.length > 0 ? argv._[0] : null;
    return {command};
  }

  static async run(args: Args): Promise<void> {
    const commandName = args.command;
    if (commandName != null) {
      const commandMap = await commandFinder(__dirname);
      const commandPath = commandMap.get(commandName);

      if (commandPath === undefined) {
        process.stderr.write(format('Unsupported command `%s`\n', commandName));
        return this.showUsage(this.BAD_ARGS);
      }

      // $FlowFixMe
      require(commandPath).default.showUsage(this.OK);
    } else {
      await this.showGeneralUsage(this.OK);
    }
  }

  static description(): string {
    return 'Shows usage message';
  }

  static async usage(): Promise<string> {
    const validCommands = await this.validCommands();

    return `usage: ${process.argv[1]} help [COMMAND]

${validCommands}
`;
  }

  static async showGeneralUsage(exitCode: number): Promise<void> {
    const validCommands = await this.validCommands();

    process.stderr.write(`usage: ${process.argv[1]} [COMMAND]

${validCommands}
`);
    process.exit(exitCode);
  }

  static async validCommands(): Promise<string> {
    const commandMap = await commandFinder(__dirname);
    const maxLength = Math.max(
      16,
      ...Array.from(commandMap.keys()).map(k => k.length),
    );

    const validCommands = [];
    for (const commandName of commandMap.keys()) {
      validCommands.push(
        format(
          '%s%s  %s',
          commandName,
          Array(maxLength - commandName.length).join(' '),
          // $FlowFixMe
          require(commandMap.get(commandName)).default.description(),
        ),
      );
    }
    return format(
      'Valid values for COMMAND:\n%s',
      validCommands.sort().join('\n'),
    );
  }
}

module.exports = {default: HelpCommand};
