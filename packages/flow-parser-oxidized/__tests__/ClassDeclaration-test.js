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
} from '../__test_utils__/parse';

describe('Super type arguments', () => {
  const code = `class C extends A<T> { }`;

  test('ESTree', () => {
    expect(parseForSnapshotESTree(code)).toMatchInlineSnapshot(`
     {
       "body": [
         {
           "body": {
             "body": [],
             "type": "ClassBody",
           },
           "decorators": [],
           "id": {
             "name": "C",
             "optional": false,
             "type": "Identifier",
             "typeAnnotation": null,
           },
           "implements": [],
           "superClass": {
             "name": "A",
             "optional": false,
             "type": "Identifier",
             "typeAnnotation": null,
           },
           "superTypeArguments": {
             "params": [
               {
                 "id": {
                   "name": "T",
                   "optional": false,
                   "type": "Identifier",
                   "typeAnnotation": null,
                 },
                 "type": "GenericTypeAnnotation",
                 "typeParameters": null,
               },
             ],
             "type": "TypeParameterInstantiation",
           },
           "type": "ClassDeclaration",
           "typeParameters": null,
         },
       ],
       "type": "Program",
     }
    `);
  });

  test('Babel', () => {
    expect(parseForSnapshotBabel(code)).toMatchInlineSnapshot(`
     {
       "body": [
         {
           "body": {
             "body": [],
             "type": "ClassBody",
           },
           "id": {
             "name": "C",
             "type": "Identifier",
           },
           "superClass": {
             "name": "A",
             "type": "Identifier",
           },
           "superTypeParameters": {
             "params": [
               {
                 "id": {
                   "name": "T",
                   "type": "Identifier",
                 },
                 "type": "GenericTypeAnnotation",
                 "typeParameters": null,
               },
             ],
             "type": "TypeParameterInstantiation",
           },
           "type": "ClassDeclaration",
         },
       ],
       "type": "Program",
     }
    `);
  });
});
