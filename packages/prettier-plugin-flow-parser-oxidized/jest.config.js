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
  automock: false,
  clearMocks: true,
  collectCoverage: false,
  setupFilesAfterEnv: ['<rootDir>/jest.setup.js'],
  testMatch: ['**/__tests__/**/*-test.js'],
  testPathIgnorePatterns: ['/node_modules/'],
  transformIgnorePatterns: ['/node_modules/'],

  // Tests import `../../.prettierrc.json` which would resolve to
  // `flow/packages/.prettierrc.json` — that file does not exist in this
  // workspace (upstream's equivalent lives at `hermes-parser/js/.prettierrc.json`,
  // a sibling shared across all upstream packages). To avoid polluting the
  // shared `flow/packages/` directory with a config that would affect other
  // sibling workspaces, we keep a package-local copy and remap the import.
  moduleNameMapper: {
    '^\\.\\./\\.\\./\\.prettierrc\\.json$': path.resolve(
      __dirname,
      '.prettierrc.json',
    ),
  },
};
