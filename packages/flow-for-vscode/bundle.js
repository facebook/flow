/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const { build } = require('esbuild');

build({
  entryPoints: ['src/index.ts'],
  bundle: true,
  minify: false,
  sourcemap: true,
  platform: 'node',
  target: 'node14',
  logLevel: 'error',
  outfile: 'build/index.js',
  external: ['vscode'],
});
