/* @flow */
import 'babel-polyfill';
import colors from 'colors/safe';

import {format} from 'util';

import commandFinder from './command/finder';

function cleanUp() {
  process.stdout.write('\x1B[?25h') // Show terminal cursor
  process.stdout.write(colors.rainbow("Cleaning up...\n"));
  process.exit(1);
}

process.on('unhandledRejection', function (err, p) {
  process.stderr.write(format("uncaught rejection\n%s\n%s\n", err, err.stack));
  cleanUp();
});

process.on('uncaughtException', function (err) {
  process.stderr.write(format("uncaught exception", err, err.stack, "\n"));
  cleanUp();
});

process.on('SIGINT', () => {
  process.stderr.write("\n\nCaught SIGINT\n");
  cleanUp();
});

export async function run(): Promise<void> {
  const commandMap = await commandFinder(__dirname);

  const command = process.argv[2];
  const commandModulePath = commandMap.get(command);
  if (command == undefined || commandModulePath == undefined) {
    const HelpCommand = require('./help/helpCommand').default;
    if (command == undefined) {
      await HelpCommand.showGeneralUsage(HelpCommand.BAD_ARGS);
    } else {
      process.stderr.write(format("Unsupported command `%s`\n", command));
      await HelpCommand.showGeneralUsage(HelpCommand.BAD_ARGS);
    }
  } else {
    /* $FlowFixMe - Babel transforms when things are required, so we only want to require the
     * command we're running */
    await require(commandModulePath).default.go();
  }
}
