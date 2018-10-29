#!/usr/bin/env node
/**
 * Copyright 2004-present Facebook. All Rights Reserved.
 *
 * @flow
 * @format
 */
const globSync = require('glob').sync;
const path = require('path');
const runner = require('./runner.js');

require('flow-remove-types/register');

var argv = require('yargs')
  .usage('Usage: $0 [options] [roots]')
  .option('bin', {
    // $FlowFixMe default does not need to be a boolean
    default: 'flow',
    describe: 'Which flow binary to use',
    nargs: 1,
    type: 'string',
  })
  .describe('bin', ' (use flow in $PATH if omitted)')
  .help('h')
  .alias('h', 'help').argv;

const bin = String(argv.bin);
let roots = argv._;

if (roots.length === 0) {
  console.log(
    'No roots specified, so looking for all .flowconfig files under %s',
    process.cwd(),
  );
  roots = globSync('**/.flowconfig', {cwd: process.cwd()}).map(path.dirname);
  console.log('Found %d root%s', roots.length, roots.length == 1 ? '' : 's');
}
process.exit(runner.run(bin, roots));
