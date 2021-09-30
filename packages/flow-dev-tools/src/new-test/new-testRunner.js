/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {join, relative, resolve} = require('path');
const {format} = require('util');

const {
  exec,
  exists,
  mkdirp,
  readFile,
  unlink,
  writeFile,
} = require('../utils/async');
const {getTestsDir, defaultFlowConfigName} = require('../constants');

import type {Args} from './new-testCommand';

async function newTest(bin: string, suiteName: string): Promise<void> {
  function log(...args: any) {
    console.log('[%s]\t\t%s', suiteName, format(...args));
  }

  const dest = join(getTestsDir(), suiteName);

  const alreadyExists = await exists(dest);

  if (alreadyExists) {
    log('There is already a test with that name. Skipping...');
    return;
  }

  await mkdirp(dest);

  const testFile = join(dest, 'test.js');
  const testerLoc = relative(dest, resolve(__dirname, '../test/Tester'));

  await writeFile(
    join(dest, 'test.js'),
    `/*
* Copyright Facebook
 * @flow

 */

import type {Suite} from 'flow-dev-tools/src/test/Suite';
const {suite, test} = require('${testerLoc}');

module.exports = (suite(({addFile, addFiles, addCode}) => [
  test('TestName', [
  ]),
]): Suite);
`,
  );

  await exec(format('%s init %s', bin, dest));

  // Rename .flowconfig to _flowconfig
  const config = await readFile(join(dest, '.flowconfig'));
  await Promise.all([
    writeFile(join(dest, defaultFlowConfigName), config.toString()),
    unlink(join(dest, '.flowconfig')),
  ]);

  log(
    'Created test! To start editing open %s',
    relative(process.cwd(), resolve(__dirname, testFile)),
  );
}

async function runner(args: Args): Promise<void> {
  await Promise.all(Array.from(args.names).map(newTest.bind(null, args.bin)));
}

module.exports = {
  default: runner,
};
