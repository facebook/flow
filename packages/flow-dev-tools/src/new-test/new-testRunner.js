/* @flow */

import {join, relative, resolve} from 'path';
import {format} from 'util';

import {exec, exists, mkdirp, readFile, unlink, writeFile} from '../utils/async';
import {getTestsDir, defaultFlowConfigName} from '../constants';

import type {Args} from './new-testCommand';

async function newTest(bin: string, suiteName: string): Promise<void> {
  function log(...args: any) {
    console.log("[%s]\t\t%s", suiteName, format(...args));
  }

  const dest = join(getTestsDir(), suiteName);

  const alreadyExists = await exists(dest);

  if (alreadyExists) {
    log("There is already a test with that name. Skipping...");
    return;
  }

  await mkdirp(dest);

  const testFile = join(dest, 'test.js');
  const testerLoc = relative(dest, resolve(__dirname, "../test/Tester"));

  await writeFile(
    join(dest, 'test.js'),
`/*
 * @flow
 */


import {suite, test} from '${testerLoc}';

export default suite(({addFile, addFiles, addCode}) => [
  test('TestName', [
  ]),
]);
`
  );

  await exec(format('%s init --options "all=true" %s', bin, dest));

  // Rename .flowconfig to _flowconfig
  const config = await readFile(join(dest, ".flowconfig"));
  await Promise.all([
    writeFile(join(dest, defaultFlowConfigName), config.toString()),
    unlink(join(dest, ".flowconfig")),
  ]);


  log(
    "Created test! To start editing open %s",
    relative(process.cwd(), resolve(__dirname, testFile)),
  );
}

export default async function(args: Args): Promise<void> {
  await Promise.all(
    Array.from(args.names).map(newTest.bind(null, args.bin))
  );
}
