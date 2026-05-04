/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {
  PropertyDefinition,
  EnumDefaultedMember,
  FunctionTypeParam,
  ComponentParameter,
  ComponentTypeParameter,
  Identifier,
  ObjectTypeIndexer,
} from 'flow-estree-oxidized';

import {
  createRemoveNodeMutation,
  performRemoveNodeMutation,
} from '../../../src/transform/mutations/RemoveNode';
import {MutationContext} from '../../../src/transform/MutationContext';
import {parseAndGetAstAndNode} from './test-utils';

describe('RemoveNode', () => {
  it('PropertyDefinition', async () => {
    const {ast, target} = await parseAndGetAstAndNode<PropertyDefinition>(
      'PropertyDefinition',
      'class Foo { prop = 1; method() {} }',
    );
    const mutation = createRemoveNodeMutation(target);
    performRemoveNodeMutation(new MutationContext(''), mutation);
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'ClassDeclaration',
          body: {
            type: 'ClassBody',
            body: [
              {
                type: 'MethodDefinition',
              },
            ],
          },
        },
      ],
    });
  });

  it('EnumDefaultedMember', async () => {
    const {ast, target} = await parseAndGetAstAndNode<EnumDefaultedMember>(
      'EnumDefaultedMember',
      'enum Foo { A, B }',
    );
    const mutation = createRemoveNodeMutation(target);
    performRemoveNodeMutation(new MutationContext(''), mutation);
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'EnumDeclaration',
          body: {
            type: 'EnumBody',
            members: [
              {
                type: 'EnumDefaultedMember',
                id: {
                  type: 'Identifier',
                  name: 'A',
                },
              },
            ],
          },
        },
      ],
    });
  });

  it('FunctionTypeParam', async () => {
    const {ast, target} = await parseAndGetAstAndNode<FunctionTypeParam>(
      'FunctionTypeParam',
      'type T = (string, number) => void',
    );
    const mutation = createRemoveNodeMutation(target);
    performRemoveNodeMutation(new MutationContext(''), mutation);
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'TypeAlias',
          right: {
            type: 'FunctionTypeAnnotation',
            params: [
              {
                type: 'FunctionTypeParam',
                typeAnnotation: {
                  type: 'StringTypeAnnotation',
                },
              },
            ],
          },
        },
      ],
    });
  });

  it('ComponentParameter', async () => {
    const {ast, target} = await parseAndGetAstAndNode<ComponentParameter>(
      'ComponentParameter',
      'component Foo(foo: string, bar: number) {}',
    );
    const mutation = createRemoveNodeMutation(target);
    performRemoveNodeMutation(new MutationContext(''), mutation);
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'ComponentDeclaration',
          params: [
            {
              type: 'ComponentParameter',
              name: {
                type: 'Identifier',
                name: 'foo',
              },
            },
          ],
        },
      ],
    });
  });

  it('ComponentTypeParameter', async () => {
    const {ast, target} = await parseAndGetAstAndNode<ComponentTypeParameter>(
      'ComponentTypeParameter',
      'type T = component(foo: string, bar: number)',
    );
    const mutation = createRemoveNodeMutation(target);
    performRemoveNodeMutation(new MutationContext(''), mutation);
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'TypeAlias',
          right: {
            type: 'ComponentTypeAnnotation',
            params: [
              {
                type: 'ComponentTypeParameter',
                typeAnnotation: {
                  type: 'StringTypeAnnotation',
                },
              },
            ],
          },
        },
      ],
    });
  });

  it('ObjectTypeIndexer', async () => {
    const {ast, target} = await parseAndGetAstAndNode<ObjectTypeIndexer>(
      'ObjectTypeIndexer',
      'type T = {prop: string, [number]: string, [string]: number};',
    );
    const mutation = createRemoveNodeMutation(target);
    performRemoveNodeMutation(new MutationContext(''), mutation);
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'TypeAlias',
          right: {
            type: 'ObjectTypeAnnotation',
            properties: [
              {
                type: 'ObjectTypeProperty',
              },
            ],
            indexers: [
              {
                type: 'ObjectTypeIndexer',
                key: {
                  type: 'NumberTypeAnnotation',
                },
                value: {
                  type: 'StringTypeAnnotation',
                },
              },
            ],
          },
        },
      ],
    });
  });

  describe('Identifier', () => {
    it('valid', async () => {
      const {ast, target} = await parseAndGetAstAndNode<Identifier>(
        'Identifier',
        '[1, a, 3];',
      );
      const mutation = createRemoveNodeMutation(target);
      performRemoveNodeMutation(new MutationContext(''), mutation);
      expect(ast).toMatchObject({
        type: 'Program',
        body: [
          {
            type: 'ExpressionStatement',
            expression: {
              type: 'ArrayExpression',
              elements: [
                {
                  type: 'Literal',
                  value: 1,
                },
                {
                  type: 'Literal',
                  value: 3,
                },
              ],
            },
          },
        ],
      });
    });

    it('invalid', async () => {
      const {target} = await parseAndGetAstAndNode<Identifier>(
        'Identifier',
        'const x = 1;',
      );
      const mutation = createRemoveNodeMutation(target);
      expect(() => performRemoveNodeMutation(new MutationContext(''), mutation))
        .toThrowErrorMatchingInlineSnapshot(`
        "Tried to remove Identifier from parent of type VariableDeclarator.
        However Identifier can only be safely removed from parent of type ArrowFunctionExpression | FunctionDeclaration | FunctionExpression | ArrayExpression | ArrayPattern."
      `);
    });
  });
});
