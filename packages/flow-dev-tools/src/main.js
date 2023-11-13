/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const chalk = require('chalk');

const {format} = require('util');

const allCommands = require('./command/allCommands');

function cleanUp() {
  process.stdout.write('\x1B[?25h'); // Show terminal cursor
  process.stdout.write(chalk.yellow('Cleaning up...\n'));
  process.exit(1);
}

process.on('unhandledRejection', function (err, p) {
  process.stderr.write(format('uncaught rejection\n%s\n%s\n', err, err.stack));
  cleanUp();
});

process.on('uncaughtException', function (err) {
  process.stderr.write(format('uncaught exception', err, err.stack, '\n'));
  cleanUp();
});

process.on('SIGINT', () => {
  process.stderr.write('\n\nCaught SIGINT\n');
  cleanUp();
});

async function run(): Promise<void> {
  const command = process.argv[2];
  const commandModule = allCommands[command];
  if (command == undefined || commandModule == undefined) {
    const HelpCommand = require('./help/helpCommand').default;
    if (command == undefined) {
      await HelpCommand.showGeneralUsage(HelpCommand.BAD_ARGS);
    } else {
      process.stderr.write(format('Unsupported command `%s`\n', command));
      await HelpCommand.showGeneralUsage(HelpCommand.BAD_ARGS);
    }
  } else {
    await commandModule().default.go();
  }
}

module.exports = {run};
