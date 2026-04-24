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

export type {TraversalContextBase, Visitor} from './traverse/traverse';
export type {TransformVisitor} from './transform/transform';
export type {TransformContext} from './transform/TransformContext';
export type {DetachedNode, MaybeDetachedNode} from './detachedNode';

export {SimpleTraverser} from 'flow-parser-oxidized';
export {traverse, traverseWithContext} from './traverse/traverse';
export {transform} from './transform/transform';
export {parse} from './transform/parse';
export {print} from './transform/print';
export * as t from './generated/node-types';
export {asDetachedNode} from './detachedNode';
export {
  cloneJSDocCommentsToNewNode,
  makeCommentOwnLine,
} from './transform/comments/comments';
