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

import {parse} from '../__test_utils__/parse';

describe('Declare implicitDeclare markers', () => {
  const parserOptions = {
    enableEnums: true,
    enableExperimentalComponentSyntax: true,
  };

  test('explicit declare forms omit implicitDeclare', () => {
    const cases = [
      ['DeclareClass', 'declare class Foo {}'],
      ['DeclareComponent', 'declare component Foo();'],
      ['DeclareFunction', 'declare function foo(): void;'],
      ['DeclareHook', 'declare hook useFoo(): void;'],
      ['DeclareVariable', 'declare const foo: number;'],
      ['DeclareExportDeclaration', 'declare export const foo: number;'],
      ['DeclareExportAllDeclaration', 'declare export * from "foo";'],
      ['DeclareOpaqueType', 'declare opaque type Foo;'],
      ['DeclareTypeAlias', 'declare type Foo = string;'],
      ['DeclareEnum', 'declare enum Foo { A }'],
      ['DeclareInterface', 'declare interface Foo {}'],
    ];

    for (const [type, source] of cases) {
      const declaration = parse(source, parserOptions).body[0];
      expect(declaration).toMatchObject({type});
      expect(declaration).not.toHaveProperty('implicitDeclare');
    }
  });

  test('ambient export declaration serializes implicitDeclare=true', () => {
    expect(
      parse('export function foo(): void;', {
        ...parserOptions,
        sourceFilename: 'test.js.flow',
      }).body[0],
    ).toMatchObject({
      type: 'DeclareExportDeclaration',
      implicitDeclare: true,
      declaration: {
        type: 'DeclareFunction',
        implicitDeclare: true,
      },
    });
  });
});
