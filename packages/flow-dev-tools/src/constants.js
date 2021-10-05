/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {dirname, join, resolve} = require('path');

const FLOW_ROOT = resolve(__dirname, '../../../');
const defaultTestsDirName = 'newtests';

// This is where we look for tests to run and where we put newly converted
// tests
function getTestsDir(relative_to?: string): string {
  if (relative_to !== undefined) {
    return resolve(relative_to, defaultTestsDirName);
  } else {
    return dirname(require.resolve(join(defaultTestsDirName, 'package.json')));
  }
}

const binOptions: Array<string> = [
  resolve(FLOW_ROOT, '../buck-out/gen/flow/flow/flow'), // Buck
  resolve(FLOW_ROOT, 'bin/flow'), // Open source build
  resolve(FLOW_ROOT, 'bin/flow.exe'), // Open source windows build
];

const defaultFlowConfigName = '_flowconfig';

module.exports = {
  defaultTestsDirName,
  getTestsDir,
  binOptions,
  defaultFlowConfigName,
};
