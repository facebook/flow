/**
 * @format
 * @flow
 */

const chalk = require('chalk');
const upgrade = require('./upgrade');

upgrade(process.cwd(), '0.52.0', '0.53.0', {all: false}).catch(error =>
  console.error(chalk.red(error ? error.stack || error : error)),
);
