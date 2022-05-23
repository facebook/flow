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

import type {CliOptions, Codemod} from '../Types';

import chalk from 'chalk';
import fs from 'fs-extra';
import yargsImport from 'yargs/yargs';
import upgrade from '../upgrade';
import path from 'path';
import {findFlowFilesWithSpinner} from '../findFlowFiles';
import runCodemods from '../runCodemods';
import Styled from '../Styled';

const CODEMOD_DIR = path.resolve(__dirname, '..', 'codemods');
async function main(args: $ReadOnlyArray<string>) {
  const codemodNames = fs
    .readdirSync(CODEMOD_DIR, {
      withFileTypes: false,
    })
    .map(f => path.parse(f).name);

  const yargs = yargsImport(args)
    .usage('Usage: flow-upgrade-codemod <codemod name>')
    .positional('codemod name', {
      describe: 'Your current flow version',
      choices: codemodNames,
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
    .demandCommand(1)
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

  const codemodName = yargs._[0];
  const directory = process.cwd();

  try {
    const filePaths = await findFlowFilesWithSpinner(directory, options);

    // $FlowExpectedError[unsupported-syntax]
    const {default: upgrade}: {default: Codemod} = await import(
      `${CODEMOD_DIR}/${codemodName}.js`
    );

    // Tell the user what we are about to do.
    if (!options.silent) {
      console.log(Styled.sectionHeader('Codemods'));
      console.log();
      console.log('We are now going to run the following codemods:');
      console.log();
      console.log(Styled.upgradeTitle(upgrade.title, 1));
      console.log();
      console.log(upgrade.description);
      console.log();
      console.log(Styled.divider());
      console.log();
      console.log(`Running ${chalk.bold.cyan(1)} codemod...`);
      console.log();
    }

    switch (upgrade.kind) {
      case 'codemod':
        await runCodemods([upgrade], filePaths, options);
        break;

      // Throw an error for all other kinds of upgrades.
      default:
        throw new Error(`Unexpected upgrade kind: '${upgrade.kind}'`);
    }
  } catch (error) {
    console.error(chalk.red(error ? error.stack || error : error));
  }
}

void main(process.argv.slice(2));
