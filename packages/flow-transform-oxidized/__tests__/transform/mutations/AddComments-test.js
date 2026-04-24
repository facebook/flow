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
import {MutationContext} from '../../../src/transform/MutationContext';
import {CommentPlacement} from '../../../src/transform/comments/comments';
import {
  createAddCommentsMutation,
  performAddCommentsMutation,
} from '../../../src/transform/mutations/AddComments';
import {parseAndGetAstAndNode} from './test-utils';

describe('AddComments', () => {
  it('Add line comment', async () => {
    const {ast, target} = await parseAndGetAstAndNode<VariableDeclaration>(
      'VariableDeclaration',
      `\
const x = 1;`,
    );

    const comment = t.LineComment({value: 'Leading Commet'});
    const mutation = createAddCommentsMutation(target, [
      {comment, placement: CommentPlacement.LEADING_OWN_LINE},
    ]);
    if (mutation == null) {
      throw new Error('Invalid state');
    }

    performAddCommentsMutation(new MutationContext(''), mutation);

    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'VariableDeclaration',
          comments: [
            {
              leading: true,
              trailing: false,
              type: 'Line',
              value: 'Leading Commet',
            },
          ],
        },
      ],
    });
  });

  it('Add block comment', async () => {
    const {ast, target} = await parseAndGetAstAndNode<VariableDeclaration>(
      'VariableDeclaration',
      `\
const x = 1;`,
    );

    const comment = t.BlockComment({value: 'Leading Commet'});
    const mutation = createAddCommentsMutation(target, [
      {comment, placement: CommentPlacement.LEADING_OWN_LINE},
    ]);
    if (mutation == null) {
      throw new Error('Invalid state');
    }

    performAddCommentsMutation(new MutationContext(''), mutation);

    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'VariableDeclaration',
          comments: [
            {
              leading: true,
              trailing: false,
              type: 'Block',
              value: 'Leading Commet',
            },
          ],
        },
      ],
    });
  });
});
