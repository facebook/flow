/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @noflow
 * @format
 */

'use strict';

const path = require('path');
const packageJson = require('./package.json');

module.exports = {
  // All imported modules in your tests should be mocked automatically
  automock: false,
  // Automatically clear mock calls and instances between every test
  clearMocks: true,
  // Indicates whether the coverage information should be collected while executing the test
  collectCoverage: false,
  // The glob patterns Jest uses to detect test files
  testMatch: ['**/__tests__/**/*-test.js'],
  // An array of regexp pattern strings that are matched against all test paths, matched tests are skipped
  testPathIgnorePatterns: ['/node_modules/'],
  // An array of regexp pattern strings that are matched against all source file paths, matched files will skip transformation
  transformIgnorePatterns: ['/node_modules/', '/dist/'],

  // These mappings tell jest how to find the source files directly instead of using the built `dist` files.
  // This allows us to run tests without first doing a build.
  //
  // We have to manually teach jest about the generated files that get placed in the `dist` folders.
  // Otherwise jest will look in the `src` folder and will only find the unusable `.flow` file.
  moduleNameMapper: {
    // the modules themselves
    ...Object.fromEntries(
      packageJson.workspaces.map(moduleName => [
        `^${moduleName}$`,
        path.resolve(__dirname, moduleName, 'src', 'index.js'),
      ]),
    ),

    // prettier-plugin-flow-parser is a pre-built bundle without src/index.js
    '^prettier-plugin-flow-parser$': path.resolve(
      __dirname,
      'prettier-plugin-flow-parser',
      'index.mjs',
    ),

    // flow-parser-oxidized
    '.*/FlowParserWASM$': path.resolve(
      __dirname,
      'flow-parser-oxidized',
      'dist',
      'FlowParserWASM.js',
    ),
  },
};
