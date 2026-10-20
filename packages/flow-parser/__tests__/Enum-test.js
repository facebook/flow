/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {AlignmentCase} from '../__test_utils__/alignment-utils';

import {expectEspreeAlignment} from '../__test_utils__/alignment-utils';
import {parseForSnapshot, printForSnapshotBabel} from '../__test_utils__/parse';
import {ident} from '../oxidized-src/utils/Builders';

describe('Enum', () => {
  test.each([
    ['enum E { A }', null, 'EnumDefaultedMember'],
    ['enum E { A = true }', null, 'EnumBooleanMember'],
    ['enum E { A = 1 }', null, 'EnumNumberMember'],
    ['enum E { A = 1n }', null, 'EnumBigIntMember'],
    ['enum E { A = "a" }', null, 'EnumStringMember'],
    ['enum E of boolean { A = true }', 'boolean', 'EnumBooleanMember'],
    ['enum E of number { A = 1 }', 'number', 'EnumNumberMember'],
    ['enum E of bigint { A = 1n }', 'bigint', 'EnumBigIntMember'],
    ['enum E of string { A = "a" }', 'string', 'EnumStringMember'],
    ['enum E of symbol { A }', 'symbol', 'EnumDefaultedMember'],
  ])('unified ESTree body: %s', (code, explicitType, memberType) => {
    for (const prefix of ['', 'declare ']) {
      expect(parseForSnapshot(prefix + code)).toMatchObject({
        body: [
          {
            body: {
              type: 'EnumBody',
              explicitType,
              hasUnknownMembers: false,
              members: [{type: memberType}],
            },
          },
        ],
      });
    }
  });

  test.each([
    ['enum E { A }', 'EnumStringBody', false],
    ['enum E { A = true }', 'EnumBooleanBody', false],
    ['enum E { A = 1 }', 'EnumNumberBody', false],
    ['enum E { A = 1n }', 'EnumBigIntBody', false],
    ['enum E of string { A = "a" }', 'EnumStringBody', true],
    ['enum E of boolean { A = true }', 'EnumBooleanBody', true],
    ['enum E of number { A = 1 }', 'EnumNumberBody', true],
    ['enum E of bigint { A = 1n }', 'EnumBigIntBody', true],
    ['enum E of symbol { A }', 'EnumSymbolBody', undefined],
  ])('preserves Babel body: %s', (code, type, explicitType) => {
    expect(parseForSnapshot(code, {babel: true})).toMatchObject({
      body: [{body: {type, ...(explicitType == null ? {} : {explicitType})}}],
    });
  });

  test('Babel symbol bodies omit explicitType', () => {
    expect(
      parseForSnapshot('enum E of symbol { A }', {babel: true}),
    ).not.toHaveProperty('body.0.body.explicitType');
  });

  test('empty ESTree bodies retain their explicit representation', () => {
    expect(parseForSnapshot('enum E of number {}')).toMatchObject({
      body: [{body: {type: 'EnumBody', explicitType: 'number', members: []}}],
    });
  });

  test('preserves unknown members and heterogeneous member nodes', () => {
    expect(parseForSnapshot('enum E { A = 1, B = "b", C, ... }')).toMatchObject(
      {
        body: [
          {
            body: {
              type: 'EnumBody',
              explicitType: null,
              hasUnknownMembers: true,
              members: [
                {type: 'EnumNumberMember'},
                {type: 'EnumStringMember'},
                {type: 'EnumDefaultedMember'},
              ],
            },
          },
        ],
      },
    );
  });

  const testCase: AlignmentCase = {
    code: `
      enum T1 { A = 1n, B = 2n}
    `,
    espree: {
      expectToFail: 'espree-exception',
      expectedExceptionMessage: `The keyword 'enum' is reserved`,
    },
    babel: {
      expectToFail: 'babel-exception',
      expectedExceptionMessage: 'Unexpected token',
    },
  };

  test('ESTree', () => {
    expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
     {
       "body": [
         {
           "body": {
             "explicitType": null,
             "hasUnknownMembers": false,
             "members": [
               {
                 "id": {
                   "name": "A",
                   "optional": false,
                   "type": "Identifier",
                   "typeAnnotation": null,
                 },
                 "init": {
                   "bigint": "1",
                   "literalType": "bigint",
                   "raw": "1n",
                   "type": "Literal",
                   "value": 1n,
                 },
                 "type": "EnumBigIntMember",
               },
               {
                 "id": {
                   "name": "B",
                   "optional": false,
                   "type": "Identifier",
                   "typeAnnotation": null,
                 },
                 "init": {
                   "bigint": "2",
                   "literalType": "bigint",
                   "raw": "2n",
                   "type": "Literal",
                   "value": 2n,
                 },
                 "type": "EnumBigIntMember",
               },
             ],
             "type": "EnumBody",
           },
           "id": {
             "name": "T1",
             "optional": false,
             "type": "Identifier",
             "typeAnnotation": null,
           },
           "type": "EnumDeclaration",
         },
       ],
       "type": "Program",
     }
    `);
    expectEspreeAlignment(testCase);
  });

  test('const enum', () => {
    expect(parseForSnapshot('const enum E {A = 1}')).toMatchObject({
      body: [{type: 'EnumDeclaration', const: true}],
    });
  });

  describe('Transform', () => {
    const options = {
      transformOptions: {TransformEnumSyntax: {enable: true}},
    };

    test('boolean', async () => {
      const code = `enum E {A = true, B = false}`;
      expect(await printForSnapshotBabel(code, options)).toMatchInlineSnapshot(`
       "const E = require("flow-enums-runtime")({
         A: true,
         B: false
       });"
      `);
    });
    test('number', async () => {
      const code = `enum E {A = 1, B = 2}`;
      expect(await printForSnapshotBabel(code, options)).toMatchInlineSnapshot(`
       "const E = require("flow-enums-runtime")({
         A: 1,
         B: 2
       });"
      `);
    });
    test('string-initialized', async () => {
      const code = `enum E {A = 'a', B = 'b'}`;
      expect(await printForSnapshotBabel(code, options)).toMatchInlineSnapshot(`
       "const E = require("flow-enums-runtime")({
         A: 'a',
         B: 'b'
       });"
      `);
    });
    test('string-defaulted', async () => {
      const code = `enum E {A, B}`;
      expect(await printForSnapshotBabel(code, options)).toMatchInlineSnapshot(
        `"const E = require("flow-enums-runtime").Mirrored(["A", "B"]);"`,
      );
    });
    test('symbol', async () => {
      const code = `enum E of symbol {A, B}`;
      expect(await printForSnapshotBabel(code, options)).toMatchInlineSnapshot(`
       "const E = require("flow-enums-runtime")({
         A: Symbol("A"),
         B: Symbol("B")
       });"
      `);
    });
    test('export', async () => {
      const code = `export enum E {A = 1, B = 2}`;
      expect(await printForSnapshotBabel(code, options)).toMatchInlineSnapshot(`
       "export const E = require("flow-enums-runtime")({
         A: 1,
         B: 2
       });"
      `);
    });
    test('export-default-program', async () => {
      const code = `export default enum E {A = 1, B = 2}`;
      expect(await printForSnapshotBabel(code, options)).toMatchInlineSnapshot(`
       "const E = require("flow-enums-runtime")({
         A: 1,
         B: 2
       });

       export default E;"
      `);
    });
    test('`getRuntime` option', async () => {
      const code = `enum E {A = 1, B = 2}`;
      const getRuntime = () => ident('Enum');
      const options = {
        transformOptions: {TransformEnumSyntax: {enable: true, getRuntime}},
      };
      expect(await printForSnapshotBabel(code, options)).toMatchInlineSnapshot(`
       "const E = Enum({
         A: 1,
         B: 2
       });"
      `);
    });
    test('option off: not transformed', async () => {
      const code = `enum E {A = true, B = false}`;
      expect(await printForSnapshotBabel(code, {})).toMatchInlineSnapshot(`
       "enum E {
         A = true,
         B = false,
       }"
      `);
      expect(
        await printForSnapshotBabel(code, {
          transformOptions: {TransformEnumSyntax: {enable: false}},
        }),
      ).toMatchInlineSnapshot(`
       "enum E {
         A = true,
         B = false,
       }"
      `);
    });
  });
});
