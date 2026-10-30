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
import mutate from '../oxidized-src/utils/mutateESTreeASTForPrettier';

// Ambient fields distinguish their initializer from an optional annotation.
test('declare-class field values reflect the presence of an annotation', () => {
  const [declaration] = parse(`
    declare class Fields {
      initialized = 1;
      annotated: number;
      both: number = 2;
    }
  `).body;
  if (declaration.type !== 'DeclareClass') {
    throw new Error('expected a declare class');
  }

  expect(declaration.body.properties).toMatchObject([
    {
      type: 'ObjectTypeProperty',
      key: {name: 'initialized'},
      value: null,
      init: {type: 'Literal', value: 1},
      method: false,
      kind: 'init',
    },
    {
      type: 'ObjectTypeProperty',
      key: {name: 'annotated'},
      value: {type: 'NumberTypeAnnotation'},
      method: false,
      kind: 'init',
    },
    {
      type: 'ObjectTypeProperty',
      key: {name: 'both'},
      value: {type: 'NumberTypeAnnotation'},
      init: {type: 'Literal', value: 2},
      method: false,
      kind: 'init',
    },
  ]); // OK
});

// Null field values do not participate in Prettier's detached-range fixup.
test('Prettier mutation skips initializer-only field values', () => {
  const program = parse('declare class Fields { initialized = 1; }');
  const declaration = program.body[0];
  if (declaration.type !== 'DeclareClass') {
    throw new Error('expected a declare class');
  }
  const property = declaration.body.properties[0];
  if (property.type !== 'ObjectTypeProperty') {
    throw new Error('expected an object type property');
  }
  property.range[0] = 1;

  expect(() => mutate(program)).not.toThrow(); // OK
  expect(property.value).toBeNull(); // OK
});

// Methods and both accessor kinds retain required function values.
test('method and accessor signatures have function values', () => {
  const [declaration] = parse(`
    declare class Signatures {
      method(): number;
      get value(): number;
      set value(value: number): void;
    }
  `).body;
  if (declaration.type !== 'DeclareClass') {
    throw new Error('expected a declare class');
  }

  expect(declaration.body.properties).toMatchObject([
    {
      value: {type: 'FunctionTypeAnnotation'},
      method: true,
      kind: 'init',
    },
    {
      value: {type: 'FunctionTypeAnnotation'},
      method: false,
      kind: 'get',
    },
    {
      value: {type: 'FunctionTypeAnnotation'},
      method: false,
      kind: 'set',
    },
  ]); // OK
});
