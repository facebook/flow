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

  // Sibling package resolution: the `flow-estree-oxidized` and
  // `flow-parser-oxidized` packages are forked under fbcode/flow/packages —
  // point jest at their src/index.js (or main entry) so we don't need a built
  // dist tree to run tests. This mirrors the moduleNameMapper trick used by
  // upstream hermes-parser/js/jest.config.js.
  moduleNameMapper: {
    '^flow-estree-oxidized$': path.resolve(
      __dirname,
      '..',
      'flow-estree-oxidized',
      'src',
      'index.js',
    ),
    '^flow-parser-oxidized$': path.resolve(
      __dirname,
      '..',
      'flow-parser-oxidized',
      'dist',
      'index.js',
    ),
  },
};
