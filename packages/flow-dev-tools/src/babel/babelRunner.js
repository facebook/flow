/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import {mkdirp, ncp, rimraf, writeFile} from '../utils/async';
import {defaultTestsDirName, getTestsDir} from '../constants';

import {
  transformFile as babel_transformFile,
  transform as transformSource
} from 'babel-core';

import {basename, dirname, extname, join, resolve} from 'path';

import type {Args} from './babelCommand';
import type {ReadStream, WriteStream} from 'fs';
import type {NCPFile} from '../utils/async';

const babelOptions = require('../../package.json').babel;
babelOptions.sourceMaps = 'inline';
babelOptions.filename = __filename;

function transformFile(filename, options) {
  return new Promise((resolve, reject) => {
    babel_transformFile(filename, options, (err, result) => {
      if (err) {
        reject(err);
      } else {
        resolve(result);
      }
    })
  });
}

async function transform(source, dest): Promise<void> {
  let {code} = await transformFile(source, babelOptions);
  await mkdirp(dirname(dest));
  await writeFile(dest, code);
}

async function transformDir(
  source: string,
  dest: string,
  shouldTransform: (file: NCPFile) => boolean,
): Promise<void> {
  await rimraf(dest);
  const transform = (read: ReadStream, write: WriteStream, file: NCPFile) => {
    if (!shouldTransform(file)) {
      return read.pipe(write);
    }
    const chunks = [];
    read.on('data', chunks.push.bind(chunks));
    read.on('end', () => {
      const {code} = transformSource(Buffer.concat(chunks), babelOptions);
      write.end(code);
    });
  }
  await ncp(source, dest, {transform, dereference: true});
}

/**
 * Copies the src directory over, transforming all the .js files
 */
async function transformTool(args: Args): Promise<void> {
  const source = resolve(__dirname, "..");
  const dest = join(args.dest, "src");
  await transformDir(source, dest, (file) => extname(file.name) == ".js");

  await writeFile(
    join(args.dest, "tool"),
`#!/usr/bin/env node
require("./src/main.js").run();
`,
  { mode: 0o777},
  );
}

/**
 * Copies the test directory over, transforming the test.js files
 */
async function transformTests(args: Args): Promise<void> {
  await transformDir(
    getTestsDir(args.src),
    join(args.dest, defaultTestsDirName),
    (file) => basename(file.name) == "test.js",
  );
}

export default async function(args: Args): Promise<void> {
  let todo;
  switch (args.transform) {
    case 'all':
      todo = [
        transformTool(args),
        transformTests(args),
      ];
      break;
    case 'tests':
      todo = [
        transformTests(args),
      ];
      break;
    case 'tool':
      todo = [
        transformTool(args),
      ];
      break;
    default:
      // Looking forward to when flow can do exhaustivity checks!
      throw new Error('Unexpected value for transform');
  }
  Promise.all(todo);
}
