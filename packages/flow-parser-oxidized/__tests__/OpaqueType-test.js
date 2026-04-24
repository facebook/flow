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
  printForSnapshotESTree,
  parseForSnapshotESTree,
  printForSnapshotBabel,
  parseForSnapshotBabel,
} from '../__test_utils__/parse';

describe('OpaqueType', () => {
  describe('Basic', () => {
    const code = `
        declare opaque type Foo super Bar extends Baz;
        opaque type Foo super Bar extends Baz = Boz;
    `;

    test('ESTree', async () => {
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
      expect(await printForSnapshotESTree(code)).toBe(`declare opaque type Foo;
opaque type Foo = Boz;`);
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "declare opaque type Foo;
        opaque type Foo = Boz;"
      `);
    });
  });
});
