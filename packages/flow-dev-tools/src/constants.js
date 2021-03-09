/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import {dirname, join, resolve} from 'path';

const FLOW_ROOT = resolve(__dirname, '../../../');
export const defaultTestsDirName = 'newtests';

// This is where we look for tests to run and where we put newly converted
// tests
export function getTestsDir(relative_to?: string): string {
  if (relative_to !== undefined) {
    return resolve(relative_to, defaultTestsDirName);
  } else {
    return dirname(require.resolve(join(defaultTestsDirName, 'package.json')));
  }
}

export const binOptions: Array<string> = [
  resolve(FLOW_ROOT, 'bin/flow'), // Open source build
  resolve(FLOW_ROOT, 'bin/flow.exe'), // Open source windows build
  resolve(FLOW_ROOT, '../buck-out/gen/flow/flow/flow'), // Buck
];

export const defaultFlowConfigName = '_flowconfig';
