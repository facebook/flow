/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {VariableDeclaration} from 'flow-estree-oxidized';

import * as t from '../../../src/generated/node-types';
import {getCommentsForNode} from '../../../src/transform/comments/comments';
import {MutationContext} from '../../../src/transform/MutationContext';
import {
  createRemoveCommentMutation,
  performRemoveCommentMutations,
} from '../../../src/transform/mutations/RemoveComment';
import {performReplaceNodeMutation} from '../../../src/transform/mutations/ReplaceNode';
import {parseAndGetAstAndNode} from './test-utils';

describe('RemoveComment', () => {
  it('removes the comment', async () => {
    const {ast, target} = await parseAndGetAstAndNode<VariableDeclaration>(
      'VariableDeclaration',
      `\
// leading comment
const x = 1;`,
    );

    const comment = getCommentsForNode(target)[0];
    const mutation = createRemoveCommentMutation(comment);
    performRemoveCommentMutations(ast, [mutation]);
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'VariableDeclaration',
          comments: [],
        },
      ],
    });
  });

  it('removes the comment if the comment is transferred via node replacement', async () => {
    const {ast, target} = await parseAndGetAstAndNode<VariableDeclaration>(
      'VariableDeclaration',
      `\
// leading comment
const x = 1;`,
    );

    const comment = getCommentsForNode(target)[0];
    const mutation = createRemoveCommentMutation(comment);

    performReplaceNodeMutation(new MutationContext(''), {
      type: 'replaceNode',
      target,
      nodeToReplaceWith: t.ExpressionStatement({
        expression: t.StringLiteral({value: 'replaced'}),
      }),
      keepComments: true,
    });

    performRemoveCommentMutations(ast, [mutation]);
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'ExpressionStatement',
          comments: [],
        },
      ],
    });
  });
});
