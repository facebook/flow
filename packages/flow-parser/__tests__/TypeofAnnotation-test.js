/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

'use strict';

import {
  parseForSnapshotBabel,
  parseForSnapshotESTree,
  printForSnapshotBabel,
} from '../__test_utils__/parse';

describe('typeof expression', () => {
  const code1 = 'type T = typeof f;';
  const code2 = 'type T = typeof f<number>;';

  test('ESTree', async () => {
    expect(await parseForSnapshotESTree(code1)).toMatchSnapshot();
    expect(await parseForSnapshotESTree(code2)).toMatchSnapshot();
  });

  test('Babel', async () => {
    expect(await parseForSnapshotBabel(code1)).toMatchSnapshot();
    expect(await parseForSnapshotBabel(code2)).toMatchSnapshot();
    expect(await printForSnapshotBabel(code1)).toMatchInlineSnapshot(
      `"type T = typeof f;"`,
    );
    expect(await printForSnapshotBabel(code2)).toMatchInlineSnapshot(
      `"type T = typeof f;"`,
    );
  });
});
