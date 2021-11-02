#!/usr/bin/env node
/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @noformat
 * @flow
 */

const chalk = require('chalk');
const upgrade = require('./upgrade');

const yargs = require('yargs/yargs')(process.argv.slice(2))
  .usage('Usage: flow-upgrade <current version> <target version>')
  .boolean(['all'])
  .describe('all', 'Include all files, not just those with the @flow pragma')
  .demandCommand(2)
  .help('help')
  .argv;

const options = {
  all: !!yargs.all,
};

// For now we are asking for the version numbers out of convenience. When we add
// upgrades for future versions we will need to check `.flowconfig` or
// `flow-bin` for the current version and allow the new version to be
// configurable. (But still default to the latest version.)
const fromVersion = yargs._[0];
const toVersion = yargs._[1];

upgrade(process.cwd(), fromVersion, toVersion, options).catch(error => {
  console.error(chalk.red(error ? error.stack || error : error));
});
