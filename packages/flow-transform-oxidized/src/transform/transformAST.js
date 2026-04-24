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

import type {ESNode, Program} from 'flow-estree-oxidized';
import type {TransformVisitor} from './transform';
import type {RemoveCommentMutation} from './mutations/RemoveComment';
import type {ParseResult} from './parse';

import {updateAllParentPointers} from '../detachedNode';
import {traverseWithContext} from '../traverse/traverse';
import {MutationContext} from './MutationContext';
import {getTransformContext} from './TransformContext';
import {performAddCommentsMutation} from './mutations/AddComments';
import {performCloneCommentsToMutation} from './mutations/CloneCommentsTo';
import {performInsertStatementMutation} from './mutations/InsertStatement';
import {performRemoveCommentMutations} from './mutations/RemoveComment';
import {performRemoveNodeMutation} from './mutations/RemoveNode';
import {performRemoveStatementMutation} from './mutations/RemoveStatement';
import {performReplaceNodeMutation} from './mutations/ReplaceNode';
import {performReplaceStatementWithManyMutation} from './mutations/ReplaceStatementWithMany';
import {performModifyNodeInPlaceMutation} from './mutations/ModifyNodeInPlace';

export type TransformASTResult = {
  ast: Program,
  astWasMutated: boolean,
  mutatedCode: string,
};

export function transformAST(
  {ast, scopeManager, code}: ParseResult,
  visitors: TransformVisitor,
): TransformASTResult {
  // traverse the AST and collect the mutations
  const transformContext = getTransformContext();
  traverseWithContext(
    code,
    ast,
    scopeManager,
    () => transformContext,
    visitors,
  );

  // apply the mutations to the AST
  const mutationContext = new MutationContext(code);

  const removeCommentMutations: Array<RemoveCommentMutation> = [];

  for (const mutation of transformContext.mutations) {
    const mutationRoot = ((): ESNode | null => {
      switch (mutation.type) {
        case 'modifyNodeInPlace': {
          return performModifyNodeInPlaceMutation(mutationContext, mutation);
        }

        case 'insertStatement': {
          return performInsertStatementMutation(mutationContext, mutation);
        }

        case 'replaceNode': {
          return performReplaceNodeMutation(mutationContext, mutation);
        }

        case 'replaceStatementWithMany': {
          return performReplaceStatementWithManyMutation(
            mutationContext,
            mutation,
          );
        }

        case 'removeNode': {
          return performRemoveNodeMutation(mutationContext, mutation);
        }

        case 'removeStatement': {
          return performRemoveStatementMutation(mutationContext, mutation);
        }

        case 'removeComment': {
          // these are handled later
          removeCommentMutations.push(mutation);
          return null;
        }

        case 'addComments': {
          return performAddCommentsMutation(mutationContext, mutation);
        }

        case 'cloneCommentsTo': {
          return performCloneCommentsToMutation(mutationContext, mutation);
        }
      }
    })();

    // ensure the subtree's parent pointers are correct
    // this is required for two reasons:
    // 1) The userland transform is just JS - so there's nothing stopping them
    //    from doing anything dodgy. The flow types have some enforcement, but
    //    ofc that can just be ignored with a suppression.
    // 2) Shallow clones are a necessary evil in the transform because they
    //    allow codemods to do simple changes to just one node without the
    //    weight that comes with deeply cloning the entire AST.
    //    However we can't update the parent pointers of the cloned node's
    //    children until the mutation step or else we would be mutating
    //    real AST nodes and potentially break the traverse step.
    //
    // Being strict here just helps us ensure we keep everything in sync
    if (mutationRoot) {
      updateAllParentPointers(mutationRoot);
    }
  }

  // remove the comments
  // this is done at the end because it requires a complete traversal of the AST
  // so that we can find relevant node's attachment array
  performRemoveCommentMutations(ast, removeCommentMutations);

  return {
    ast,
    astWasMutated: transformContext.astWasMutated,
    mutatedCode: mutationContext.code,
  };
}
