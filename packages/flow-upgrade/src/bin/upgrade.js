#!/usr/bin/env node
/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {CliOptions} from '../Types';

import chalk from 'chalk';
import fs from 'fs-extra';
import yargsImport from 'yargs/yargs';
import upgrade from '../upgrade';

async function main(args: $ReadOnlyArray<string>) {
  const yargs = yargsImport(args)
    .usage('Usage: flow-upgrade <current version> <target version>')
    .positional('current version', {
      describe: 'Your current flow version',
    })
    .positional('target version', {
      describe: 'The flow version you are upgrading to',
    })
    .options({
      all: {
        type: 'boolean',
        describe: 'Include all files, not just those with the @flow pragma',
      },
      prettierrc: {
        type: 'string',
        describe: 'The path to a `.prettierrc` file for formatting the output.',
        requiresArg: true,
      },
      silent: {
        type: 'boolean',
        describe: 'Disable console log output.',
      },
      yes: {
        type: 'boolean',
        describe: 'Answer yes to all prompts.',
      },
    })
    .demandCommand(2)
    .help('help').argv;

  const prettierOptions = await (() => {
    if (!yargs.prettierrc) {
      return {};
    }

    return fs.readJSON(yargs.prettierrc);
  })();

  const options: CliOptions = {
    all: !!yargs.all,
    prettierOptions,
    silent: !!yargs.silent,
    yes: !!yargs.yes,
  };

  // For now we are asking for the version numbers out of convenience. When we add
  // upgrades for future versions we will need to check `.flowconfig` or
  // `flow-bin` for the current version and allow the new version to be
  // configurable. (But still default to the latest version.)
  const fromVersion = yargs._[0];
  const toVersion = yargs._[1];

  try {
    await upgrade(process.cwd(), fromVersion, toVersion, options);
  } catch (error) {
    console.error(chalk.red(error ? error.stack || error : error));
  }
}

void main(process.argv.slice(2));
