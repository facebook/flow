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

import type {AlignmentCase} from '../__test_utils__/alignment-utils';

import {
  expectBabelAlignment,
  expectEspreeAlignment,
} from '../__test_utils__/alignment-utils';
import {parseForSnapshot} from '../__test_utils__/parse';

describe('Private properties', () => {
  describe('Property Definition', () => {
    const testCase: AlignmentCase = {
      code: `
        class Foo {
          #private() {}
        }
      `,
      espree: {expectToFail: false},
      babel: {expectToFail: false},
    };

    test('ESTree', () => {
      expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
       {
         "body": [
           {
             "body": {
               "body": [
                 {
                   "computed": false,
                   "decorators": [],
                   "key": {
                     "name": "private",
                     "type": "PrivateIdentifier",
                   },
                   "kind": "method",
                   "static": false,
                   "type": "MethodDefinition",
                   "value": {
                     "async": false,
                     "body": {
                       "body": [],
                       "type": "BlockStatement",
                     },
                     "expression": false,
                     "generator": false,
                     "id": null,
                     "params": [],
                     "predicate": null,
                     "returnType": null,
                     "type": "FunctionExpression",
                     "typeParameters": null,
                   },
                 },
               ],
               "type": "ClassBody",
             },
             "decorators": [],
             "id": {
               "name": "Foo",
               "optional": false,
               "type": "Identifier",
               "typeAnnotation": null,
             },
             "implements": [],
             "superClass": null,
             "superTypeArguments": null,
             "type": "ClassDeclaration",
             "typeParameters": null,
           },
         ],
         "type": "Program",
       }
      `);
      expectEspreeAlignment(testCase);
    });

    test('Babel', () => {
      expectBabelAlignment(testCase);
    });
  });

  describe('Member Expression', () => {
    const testCase: AlignmentCase = {
      code: `
        class Foo {
          #private() {}
          constructor() {
            foo.#private();
          }
        }
      `,
      espree: {expectToFail: false},
      babel: {expectToFail: false},
    };

    test('ESTree', () => {
      expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
       {
         "body": [
           {
             "body": {
               "body": [
                 {
                   "computed": false,
                   "decorators": [],
                   "key": {
                     "name": "private",
                     "type": "PrivateIdentifier",
                   },
                   "kind": "method",
                   "static": false,
                   "type": "MethodDefinition",
                   "value": {
                     "async": false,
                     "body": {
                       "body": [],
                       "type": "BlockStatement",
                     },
                     "expression": false,
                     "generator": false,
                     "id": null,
                     "params": [],
                     "predicate": null,
                     "returnType": null,
                     "type": "FunctionExpression",
                     "typeParameters": null,
                   },
                 },
                 {
                   "computed": false,
                   "decorators": [],
                   "key": {
                     "name": "constructor",
                     "optional": false,
                     "type": "Identifier",
                     "typeAnnotation": null,
                   },
                   "kind": "constructor",
                   "static": false,
                   "type": "MethodDefinition",
                   "value": {
                     "async": false,
                     "body": {
                       "body": [
                         {
                           "directive": null,
                           "expression": {
                             "arguments": [],
                             "callee": {
                               "computed": false,
                               "object": {
                                 "name": "foo",
                                 "optional": false,
                                 "type": "Identifier",
                                 "typeAnnotation": null,
                               },
                               "optional": false,
                               "property": {
                                 "name": "private",
                                 "type": "PrivateIdentifier",
                               },
                               "type": "MemberExpression",
                             },
                             "optional": false,
                             "type": "CallExpression",
                             "typeArguments": null,
                           },
                           "type": "ExpressionStatement",
                         },
                       ],
                       "type": "BlockStatement",
                     },
                     "expression": false,
                     "generator": false,
                     "id": null,
                     "params": [],
                     "predicate": null,
                     "returnType": null,
                     "type": "FunctionExpression",
                     "typeParameters": null,
                   },
                 },
               ],
               "type": "ClassBody",
             },
             "decorators": [],
             "id": {
               "name": "Foo",
               "optional": false,
               "type": "Identifier",
               "typeAnnotation": null,
             },
             "implements": [],
             "superClass": null,
             "superTypeArguments": null,
             "type": "ClassDeclaration",
             "typeParameters": null,
           },
         ],
         "type": "Program",
       }
      `);
      expectEspreeAlignment(testCase);
    });

    test('Babel', () => {
      expectBabelAlignment(testCase);
    });
  });
});
