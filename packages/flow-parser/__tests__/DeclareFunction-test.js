/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import {
  cleanASTForSnapshot,
  parse,
  parseForSnapshotESTree,
} from '../__test_utils__/parse';

describe('DeclareFunction', () => {
  test('explicit declare', () => {
    expect(
      parseForSnapshotESTree('declare function foo(): void;'),
    ).toMatchSnapshot();
  });

  test('ambient export function', () => {
    expect(
      cleanASTForSnapshot(
        parse('export function foo(): void;', {
          sourceFilename: 'test.js.flow',
        }),
      ),
    ).toMatchSnapshot();
  });
});
