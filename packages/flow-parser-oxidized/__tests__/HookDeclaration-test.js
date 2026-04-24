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

describe('HookDeclaration', () => {
  describe('Basic', () => {
    const code = `
      hook useFoo() {}
    `;

    test('ESTree', async () => {
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
        `"function useFoo() {}"`,
      );
    });
  });

  describe('Params', () => {
    const code = `
      hook useFoo(bar: Bar, baz: Baz, bav: Bav) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
        `"function useFoo(bar: Bar, baz: Baz, bav: Bav) {}"`,
      );
    });
  });

  describe('default params', () => {
    const code = `
      hook useFoo(bar: Bar = '') {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
        `"function useFoo(bar: Bar = '') {}"`,
      );
    });
  });

  describe('return type', () => {
    const code = `
      hook useFoo(): number {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
        `"function useFoo(): number {}"`,
      );
    });
  });

  describe('type parameters', () => {
    const code = `
      hook useFoo<T1, T2>(bar: T1): T2 {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
        `"function useFoo<T1, T2>(bar: T1): T2 {}"`,
      );
    });
  });

  describe('rest params', () => {
    const code = `
      hook useFoo1(...props: Props) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
        `"function useFoo1(...props: Props) {}"`,
      );
    });
  });

  describe('rest params 2', () => {
    const code = `
      hook useFoo2(...{prop}: Props) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
          "function useFoo2(...{
            prop
          }: Props) {}"
        `);
    });
  });

  describe('Export default hook', () => {
    const code = `
      export default hook useFoo2(...{prop}: Props) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
          "export default function useFoo2(...{
            prop
          }: Props) {}"
        `);
    });
  });

  describe('Export named hook', () => {
    const code = `
      export hook useFoo2(...{prop}: Props) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
          "export function useFoo2(...{
            prop
          }: Props) {}"
        `);
    });
  });

  describe('async', () => {
    const code = `
      async hook useFoo() {}
    `;

    test('ESTree', async () => {
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
      // TODO: Enable print round-trip test once prettier fork supports
      // async hook syntax.
      // expect(await printForSnapshotESTree(code)).toBe(code.trim());
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
        `"async function useFoo() {}"`,
      );
    });
  });
});
