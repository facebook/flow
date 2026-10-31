/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {ReadonlyTypeOperator} from 'flow-estree';

import {
  parse,
  printForSnapshotESTree,
  parseForSnapshotESTree,
  printForSnapshotBabel,
  parseForSnapshotBabel,
} from '../__test_utils__/parse';

// Readonly operators narrow to their public interface and retain their operand.
test.each([
  ['type T = readonly string[];', 'ArrayTypeAnnotation'],
  ['type T = readonly [string, number];', 'TupleTypeAnnotation'],
])('readonly ESTree type: %s', (code, operandType) => {
  const declaration = parse(code).body[0];
  if (
    declaration.type !== 'TypeAlias' ||
    declaration.right.type !== 'TypeOperator' ||
    declaration.right.operator !== 'readonly'
  ) {
    throw new Error('Expected a readonly type operator');
  }
  const operator: ReadonlyTypeOperator = declaration.right;
  expect(operator.operator).toBe('readonly'); // OK
  expect(operator.typeAnnotation.type).toBe(operandType); // OK
});

describe('TypeOperator', () => {
  describe('renders', () => {
    describe('Basic', () => {
      const code = `
type T1 = renders Foo;
type T2 = renders* Foo;
type T3 = renders? Foo;
      `;

      test('ESTree', async () => {
        expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
        expect(await printForSnapshotESTree(code)).toBe(code.trim());
      });

      test('Babel', async () => {
        expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
        expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
          "type T1 = any;
          type T2 = any;
          type T3 = any;"
        `);
      });
    });
    describe('Union', () => {
      const code = `
type T1 = renders (Foo | Bar);
type T2 = renders* (Foo | Bar);
type T3 = renders? (Foo | Bar);
      `;

      test('ESTree', async () => {
        expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
        expect(await printForSnapshotESTree(code)).toBe(code.trim());
      });

      test('Babel', async () => {
        expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
        expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
          "type T1 = any;
          type T2 = any;
          type T3 = any;"
        `);
      });
    });
    describe('Nested Union', () => {
      const code = `
        type T = renders (Foo | Bar) | null;
      `;

      test('ESTree', async () => {
        expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
        expect(await printForSnapshotESTree(code)).toBe(code.trim());
      });

      test('Babel', async () => {
        expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
        expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
          `"type T = any | null;"`,
        );
      });
    });
  });
});
