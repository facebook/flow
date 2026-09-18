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
  parse,
  printForSnapshotESTree,
  parseForSnapshotESTree,
  printForSnapshotBabel,
  parseForSnapshotBabel,
} from '../__test_utils__/parse';

describe('MappedType', () => {
  describe('Basic', () => {
    const code = `
      type Mapped = {[key in keyof O]: O[key]};
    `;

    test('ESTree', async () => {
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
        `"type Mapped = any;"`,
      );
    });
  });

  describe('Union', () => {
    const code = `
      type Mapped = {[key in keyof (O | Z)]: O[key]};
    `;

    test('ESTree', async () => {
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
      expect(await printForSnapshotESTree(code)).toBe(
        'type Mapped = {[key in keyof (O | Z)]: O[key]};',
      );
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
        `"type Mapped = any;"`,
      );
    });
  });

  describe('readonly modifiers and key remapping', () => {
    function parseMappedTypeProperty(code: string) {
      const [statement] = parse(code).body;
      if (
        statement.type !== 'TypeAlias' ||
        statement.right.type !== 'ObjectTypeAnnotation' ||
        statement.right.properties[0]?.type !== 'ObjectTypeMappedTypeProperty'
      ) {
        throw new Error('expected a mapped type property');
      }
      return statement.right.properties[0];
    }

    test('preserves readonly', () => {
      const property = parseMappedTypeProperty(
        'type Plain<T> = {readonly [K in keyof T]: T[K]};',
      );

      expect(property).toMatchObject({
        nameType: null,
        variance: {kind: 'readonly'},
        varianceOp: null,
      });
    });

    test('preserves +readonly', () => {
      const property = parseMappedTypeProperty(
        'type Add<T> = {+readonly [K in keyof T]: T[K]};',
      );

      expect(property).toMatchObject({
        nameType: null,
        variance: {kind: 'readonly'},
        varianceOp: '+',
      });
      expect(property.variance?.range[0]).toBe(property.range[0]);
    });

    test('preserves -readonly', () => {
      const property = parseMappedTypeProperty(
        'type Remove<T> = {-readonly [K in keyof T]: T[K]};',
      );

      expect(property).toMatchObject({
        nameType: null,
        variance: {kind: 'readonly'},
        varianceOp: '-',
      });
      expect(property.variance?.range[0]).toBe(property.range[0]);
    });

    test('preserves key remapping', () => {
      const property = parseMappedTypeProperty(
        'type Remap<T> = {[K in keyof T as K]: T[K]};',
      );

      expect(property).toMatchObject({
        nameType: {type: 'GenericTypeAnnotation'},
        variance: null,
        varianceOp: null,
      });
    });
  });
});
