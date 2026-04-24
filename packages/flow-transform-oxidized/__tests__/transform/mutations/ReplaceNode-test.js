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
  AnyTypeAnnotation,
  Identifier,
  VariableDeclaration,
} from 'flow-estree-oxidized';

import * as t from '../../../src/generated/node-types';
import {
  createReplaceNodeMutation,
  performReplaceNodeMutation,
} from '../../../src/transform/mutations/ReplaceNode';
import {MutationContext} from '../../../src/transform/MutationContext';
import {parseAndGetAstAndNode} from './test-utils';

describe('ReplaceNode', () => {
  it('Identifier', async () => {
    const {ast, target} = await parseAndGetAstAndNode<Identifier>(
      'Identifier',
      'const x = 1;',
    );
    const mutation = createReplaceNodeMutation(
      target,
      t.Identifier({name: 'y'}),
    );
    performReplaceNodeMutation(new MutationContext(''), mutation);
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'VariableDeclaration',
          declarations: [
            {
              type: 'VariableDeclarator',
              id: {
                type: 'Identifier',
                name: 'y',
              },
            },
          ],
        },
      ],
    });
  });

  it('AnyTypeAnnotation', async () => {
    const {ast, target} = await parseAndGetAstAndNode<AnyTypeAnnotation>(
      'AnyTypeAnnotation',
      'const x: any = 1;',
    );
    const mutation = createReplaceNodeMutation(
      target,
      t.NumberTypeAnnotation(),
    );
    performReplaceNodeMutation(new MutationContext(''), mutation);
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'VariableDeclaration',
          declarations: [
            {
              type: 'VariableDeclarator',
              id: {
                type: 'Identifier',
                name: 'x',
                typeAnnotation: {
                  type: 'TypeAnnotation',
                  typeAnnotation: {
                    type: 'NumberTypeAnnotation',
                  },
                },
              },
            },
          ],
        },
      ],
    });
  });

  it('VariableDeclaration', async () => {
    const {ast, target} = await parseAndGetAstAndNode<VariableDeclaration>(
      'VariableDeclaration',
      'const x = 1, y = 2;',
    );
    const mutation = createReplaceNodeMutation(
      target,
      t.ExpressionStatement({expression: t.BooleanLiteral({value: true})}),
    );
    performReplaceNodeMutation(new MutationContext(''), mutation);
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'ExpressionStatement',
          expression: {
            type: 'Literal',
            value: true,
          },
        },
      ],
    });
  });
});
