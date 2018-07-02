#!/usr/bin/env node

/**
 * @format
 * @flow
 */

const yargs = require('yargs').argv;
const chalk = require('chalk');
const upgrade = require('./upgrade');

const options = {
  all: !!yargs.all,
};

// For now we are hardcoding the version numbers out of convenience. When we add
// upgrades for future versions we will need to check `.flowconfig` or
// `flow-bin` for the current version and allow the new version to be
// configurable. (But still default to the latest version.)
const fromVersion = '0.52.0';
const toVersion = '0.53.0';

upgrade(process.cwd(), fromVersion, toVersion, options).catch(error => {
  console.error(chalk.red(error ? error.stack || error : error));
});
