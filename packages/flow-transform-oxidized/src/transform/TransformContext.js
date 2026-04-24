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
  ClassMember,
  Comment,
  ESNode,
  Expression,
  FunctionParameter,
  ModuleDeclaration,
  Statement,
  TypeAnnotationType,
} from 'flow-estree-oxidized';
import type {DetachedNode, MaybeDetachedNode} from '../detachedNode';
import type {TransformCloneSignatures} from '../generated/TransformCloneSignatures';
import type {TransformModifySignatures} from '../generated/TransformModifySignatures';
import type {TransformReplaceSignatures} from '../generated/TransformReplaceSignatures';
import type {TraversalContext} from '../traverse/traverse';
import type {AddCommentsMutation} from './mutations/AddComments';
import type {CloneCommentsToMutation} from './mutations/CloneCommentsTo';
import type {InsertStatementMutation} from './mutations/InsertStatement';
import type {RemoveCommentMutation} from './mutations/RemoveComment';
import type {RemoveNodeMutation} from './mutations/RemoveNode';
import type {RemoveStatementMutation} from './mutations/RemoveStatement';
import type {ReplaceNodeMutation} from './mutations/ReplaceNode';
import type {ModifyNodeInPlaceMutation} from './mutations/ModifyNodeInPlace';
import type {
  ReplaceStatementWithManyMutation,
  ReplaceStatementWithManyMutationNodes,
} from './mutations/ReplaceStatementWithMany';

import {asDetachedNode, deepCloneNode, shallowCloneNode} from '../detachedNode';
import {isNode} from 'flow-parser-oxidized';
import {
  CommentPlacement,
  getCommentsForNode,
  isLeadingComment,
  isTrailingComment,
} from './comments/comments';
import {createAddCommentsMutation} from './mutations/AddComments';
import {createCloneCommentsToMutation} from './mutations/CloneCommentsTo';
import {createInsertStatementMutation} from './mutations/InsertStatement';
import {createRemoveCommentMutation} from './mutations/RemoveComment';
import {createRemoveNodeMutation} from './mutations/RemoveNode';
import {createRemoveStatementMutation} from './mutations/RemoveStatement';
import {createReplaceNodeMutation} from './mutations/ReplaceNode';
import {createReplaceStatementWithManyMutation} from './mutations/ReplaceStatementWithMany';
import {createModifyNodeInPlaceMutation} from './mutations/ModifyNodeInPlace';

type Mutation = $ReadOnly<
  | AddCommentsMutation
  | CloneCommentsToMutation
  | InsertStatementMutation
  | RemoveCommentMutation
  | RemoveNodeMutation
  | RemoveStatementMutation
  | ReplaceNodeMutation
  | ReplaceStatementWithManyMutation
  | ModifyNodeInPlaceMutation,
>;

type SingleOrArray<+T> = T | $ReadOnlyArray<T>;

type ReplaceNodeOptions = $ReadOnly<{
  /**
   * Moves the comments from the target node to the nodetoReplaceWith.
   * Note that this does not *clone* comments, it moves them and clears out
   * the target node's comments afterward.
   */
  keepComments?: boolean,
}>;

type TransformCloneAPIs = $ReadOnly<{
  /**
   * Shallowly clones the given node.
   *
   * !!! Be careful about using this !!!
   * This does not clone children nodes. This means that if you keep
   * the original node in the AST then you will have two trees in the
   * AST which refer to the exact same node objects; which will lead
   * to ***undefined*** behaviour.
   *
   * You should only use this if:
   * 1) the node is a simple leaf (eg literals, identifiers, etc)
   * 2) you are 100% removing the original node from the AST
   *
   * If you want to literally duplicate a node to place somewhere else
   * in the AST, then use `deepCloneNode` instead.
   */
  shallowCloneNode: {
    <T: ESNode>(node: T): DetachedNode<T>,
    <T: ESNode>(node: ?T): DetachedNode<T> | null,
  },

  /**
   * Shallowly clones the given node and applies the given overrides.
   * {@see shallowCloneNode}
   */
  shallowCloneNodeWithOverrides: TransformCloneSignatures,

  /**
   * {@see shallowCloneNode}
   */
  shallowCloneArray: {
    <T: ESNode>(node: $ReadOnlyArray<T>): $ReadOnlyArray<DetachedNode<T>>,
    <T: ESNode>(node: ?$ReadOnlyArray<T>): ?$ReadOnlyArray<DetachedNode<T>>,
    <T: ESNode>(node: $ReadOnlyArray<?T>): $ReadOnlyArray<DetachedNode<?T>>,
    <T: ESNode>(node: ?$ReadOnlyArray<?T>): ?$ReadOnlyArray<DetachedNode<?T>>,
  },

  /**
   * Deeply clones the node and all its children.
   *
   * !!! Be careful about using this !!!
   * Because this is a deep clone, using it high up in the AST can
   * result in a lot of work being done.
   */
  deepCloneNode: {
    <T: ESNode>(node: T): DetachedNode<T>,
    <T: ESNode>(node: ?T): DetachedNode<T> | null,
  },

  /**
   * Deeply clones the node and all its children, then applies the
   * given overrides.
   * {@see deepCloneNode}
   */
  deepCloneNodeWithOverrides: TransformCloneSignatures,
}>;

type TransformCommentAPIs = $ReadOnly<{
  /**
   * Gets all of the comments attached to the given node.
   */
  getComments: (node: ESNode) => Array<Comment>,

  /**
   * Gets the leading comments attached to the given node.
   */
  getLeadingComments: (node: ESNode) => Array<Comment>,

  /**
   * Gets the trailing comments attached to the given node.
   */
  getTrailingComments: (node: ESNode) => Array<Comment>,

  /**
   * Clones all of the comments from the target node to the destination node.
   * As its name suggest - this will clone the comments, duplicating them
   * entirely. It will not remove the comments from the target node afterward.
   */
  cloneCommentsTo: (
    target: ESNode,
    destination: MaybeDetachedNode<ESNode>,
  ) => void,

  /**
   * Add comments on the line before a specified node.
   */
  addLeadingComments: (
    node: MaybeDetachedNode<ESNode>,
    comments: SingleOrArray<Comment>,
  ) => void,

  /**
   * Add comments inline before a specified node.
   */
  addLeadingInlineComments: (
    node: MaybeDetachedNode<ESNode>,
    comments: SingleOrArray<Comment>,
  ) => void,

  /**
   * Add comments on the line after a specified node.
   */
  addTrailingComments: (
    node: MaybeDetachedNode<ESNode>,
    comments: SingleOrArray<Comment>,
  ) => void,

  /**
   * Add comments inline after a specified node.
   */
  addTrailingInlineComments: (
    node: MaybeDetachedNode<ESNode>,
    comments: SingleOrArray<Comment>,
  ) => void,

  /**
   * Removes the specified comments
   */
  removeComments: (comments: SingleOrArray<Comment>) => void,
}>;

type TransformInsertAPIs = $ReadOnly<{
  /**
   * Insert `nodeToInsert` after the `target` statement.
   * The inserted nodes will be kept in the order given.
   */
  insertAfterStatement: (
    target: InsertStatementMutation['target'],
    nodeToInsert: SingleOrArray<
      MaybeDetachedNode<InsertStatementMutation['target']>,
    >,
  ) => void,

  /**
   * Insert `nodeToInsert` before the `target` statement.
   * The inserted nodes will be kept in the order given.
   */
  insertBeforeStatement: (
    target: InsertStatementMutation['target'],
    nodeToInsert: SingleOrArray<
      MaybeDetachedNode<InsertStatementMutation['target']>,
    >,
  ) => void,
}>;

type TransformModifyAPIs = $ReadOnly<{
  /**
   * Modifies a given node in place.
   * This is equivalent to doing a replace with a shallow clone with overrides.
   */
  modifyNodeInPlace: TransformModifySignatures,
}>;

type TransformRemoveAPIs = $ReadOnly<{
  /**
   * Removes a given node from the AST.
   * The set of thigns that can be removed is intentionally restricted by types.
   * This represents the set of "misc nodes" that are known to be safe to remove without outright breaking the AST.
   */
  removeNode: (node: RemoveNodeMutation['node']) => void,

  /**
   * Removes a given statement from the AST.
   */
  removeStatement: (node: RemoveStatementMutation['node']) => void,
}>;

type TransformReplaceAPIs = $ReadOnly<{
  /**
   * Replace the `target` node with the `nodeToReplaceWith` node.
   * This simply does an in-place replacement in the AST.
   */
  replaceNode: {
    // Expressions may be replaced with other expressions
    (
      target: Expression,
      nodeToReplaceWith: MaybeDetachedNode<Expression>,
      options?: ReplaceNodeOptions,
    ): void,
    // Module declarations may be replaced with statements or other module declarations
    (
      target: ModuleDeclaration,
      nodeToReplaceWith: MaybeDetachedNode<ModuleDeclaration | Statement>,
      options?: ReplaceNodeOptions,
    ): void,
    // Statement maybe be replaced with statements or module declarations
    (
      target: Statement,
      nodeToReplaceWith: MaybeDetachedNode<ModuleDeclaration | Statement>,
      options?: ReplaceNodeOptions,
    ): void,
    // Types maybe be replaced with other types
    (
      target: TypeAnnotationType,
      nodeToReplaceWith: MaybeDetachedNode<TypeAnnotationType>,
      options?: ReplaceNodeOptions,
    ): void,
    // Class members may be replaced with other class members
    (
      target: ClassMember,
      nodeToReplaceWith: MaybeDetachedNode<ClassMember>,
      options?: ReplaceNodeOptions,
    ): void,
    // Function params amy be replace with other function params
    (
      target: FunctionParameter,
      nodeToReplaceWith: MaybeDetachedNode<FunctionParameter>,
      options?: ReplaceNodeOptions,
    ): void,
  } & TransformReplaceSignatures, // allow like-for-like replacements as well

  /**
   * Replaces the `target` node with all of the `nodesToReplaceWith` nodes.
   * The nodes will be kept in the order given.
   */
  replaceStatementWithMany: (
    target: ReplaceStatementWithManyMutation['target'],
    nodesToReplaceWith: $ReadOnlyArray<
      MaybeDetachedNode<ReplaceStatementWithManyMutationNodes>,
    >,
    options?: {
      /**
       * Moves the comments from the target node to the first node in the array.
       * Note that this does not *clone* comments, it moves them and clears out
       * the target node's comments afterward.
       */
      keepComments?: boolean,
    },
  ) => void,
}>;

export type TransformContextAdditions = $ReadOnly<{
  mutations: $ReadOnlyArray<Mutation>,
  astWasMutated: boolean,

  ...TransformCommentAPIs,
  ...TransformCloneAPIs,
  ...TransformInsertAPIs,
  ...TransformModifyAPIs,
  ...TransformRemoveAPIs,
  ...TransformReplaceAPIs,
}>;
export type TransformContext = TraversalContext<TransformContextAdditions>;

export function getTransformContext(): TransformContextAdditions {
  /**
   * The mutations in order of collection.
   */
  const mutations: Array<Mutation> = [];
  function pushMutation(mutation: ?Mutation): void {
    if (mutation != null) {
      mutations.push(mutation);
    }
  }

  const cloneAPIs: TransformCloneAPIs = {
    // $FlowFixMe[incompatible-exact]
    shallowCloneNode: (((node: ?ESNode): ?DetachedNode<ESNode> => {
      if (node == null) {
        return null;
      }

      return shallowCloneNode(node, {});
    }: $FlowFixMe): TransformCloneAPIs['shallowCloneNode']),

    shallowCloneNodeWithOverrides: (((
      node: ?ESNode,
      newProps?: $ReadOnly<{...}> = {},
    ): // $FlowExpectedError[prop-missing]
    ?DetachedNode<ESNode> => {
      if (node == null) {
        return null;
      }

      return shallowCloneNode(node, newProps);
    }: $FlowFixMe): TransformCloneAPIs['shallowCloneNodeWithOverrides']),

    // $FlowFixMe[incompatible-exact]
    shallowCloneArray: ((<T: ESNode>(
      nodes: ?$ReadOnlyArray<?T>,
    ): ?$ReadOnlyArray<DetachedNode<?ESNode>> => {
      if (nodes == null) {
        return null;
      }

      return nodes.map((node_: ?T): DetachedNode<?ESNode> => {
        const node: ?ESNode = node_;
        if (node == null) {
          // $FlowExpectedError[incompatible-type]
          return node;
        }
        return shallowCloneNode<ESNode>(node, {});
      });
    }: $FlowFixMe): TransformCloneAPIs['shallowCloneArray']),

    // $FlowFixMe[incompatible-exact]
    deepCloneNode: (((node: ?ESNode): ?DetachedNode<ESNode> => {
      if (node == null) {
        return null;
      }

      return deepCloneNode(node, {});
    }: $FlowFixMe): TransformCloneAPIs['deepCloneNode']),

    deepCloneNodeWithOverrides: (((
      node: ?ESNode,
      newProps?: $ReadOnly<{...}> = {},
    ): // $FlowExpectedError[prop-missing]
    ?DetachedNode<ESNode> => {
      if (node == null) {
        return null;
      }

      return deepCloneNode(node, newProps);
    }: $FlowFixMe): TransformCloneAPIs['deepCloneNodeWithOverrides']),
  };
  const commentAPIs: TransformCommentAPIs = {
    getComments: ((node): Array<Comment> => {
      return [...getCommentsForNode(node)];
    }: TransformCommentAPIs['getComments']),

    getLeadingComments: ((node): Array<Comment> => {
      return getCommentsForNode(node).filter(isLeadingComment);
    }: TransformCommentAPIs['getLeadingComments']),

    getTrailingComments: ((node): Array<Comment> => {
      return getCommentsForNode(node).filter(isTrailingComment);
    }: TransformCommentAPIs['getTrailingComments']),

    cloneCommentsTo: ((target, destination): void => {
      pushMutation(createCloneCommentsToMutation(target, destination));
    }: TransformCommentAPIs['cloneCommentsTo']),

    addLeadingComments: ((node, comments): void => {
      pushMutation(
        createAddCommentsMutation(
          node,
          toArray(comments).map(comment => ({
            comment,
            placement: CommentPlacement.LEADING_OWN_LINE,
          })),
        ),
      );
    }: TransformCommentAPIs['addLeadingComments']),

    addLeadingInlineComments: ((node, comments): void => {
      pushMutation(
        createAddCommentsMutation(
          node,
          toArray(comments).map(comment => ({
            comment,
            placement: CommentPlacement.LEADING_INLINE,
          })),
        ),
      );
    }: TransformCommentAPIs['addLeadingInlineComments']),

    addTrailingComments: ((node, comments): void => {
      pushMutation(
        createAddCommentsMutation(
          node,
          toArray(comments).map(comment => ({
            comment,
            placement: CommentPlacement.TRAILING_OWN_LINE,
          })),
        ),
      );
    }: TransformCommentAPIs['addTrailingComments']),

    addTrailingInlineComments: ((node, comments): void => {
      pushMutation(
        createAddCommentsMutation(
          node,
          toArray(comments).map(comment => ({
            comment,
            placement: CommentPlacement.TRAILING_INLINE,
          })),
        ),
      );
    }: TransformCommentAPIs['addTrailingInlineComments']),

    removeComments: ((comments): void => {
      toArray(comments).forEach(comment => {
        pushMutation(createRemoveCommentMutation(comment));
      });
    }: TransformCommentAPIs['removeComments']),
  };
  const insertAPIs: TransformInsertAPIs = {
    insertAfterStatement: ((target, nodesToInsert): void => {
      pushMutation(
        createInsertStatementMutation(
          'after',
          target,
          toArray(nodesToInsert).map(n =>
            asDetachedNode(n, {useDeepClone: true}),
          ),
        ),
      );
    }: TransformInsertAPIs['insertBeforeStatement']),

    insertBeforeStatement: ((target, nodesToInsert): void => {
      pushMutation(
        createInsertStatementMutation(
          'before',
          target,
          toArray(nodesToInsert).map(n =>
            asDetachedNode(n, {useDeepClone: true}),
          ),
        ),
      );
    }: TransformInsertAPIs['insertBeforeStatement']),
  };
  const removeAPIs: TransformRemoveAPIs = {
    removeNode: ((node): void => {
      pushMutation(createRemoveNodeMutation(node));
    }: TransformRemoveAPIs['removeNode']),

    removeStatement: ((node): void => {
      pushMutation(createRemoveStatementMutation(node));
    }: TransformRemoveAPIs['removeStatement']),
  };
  const replaceAPIs: TransformReplaceAPIs = {
    replaceNode: (((
      target: ESNode,
      nodeToReplaceWith: MaybeDetachedNode<ESNode>,
      options?: ReplaceNodeOptions,
    ): void => {
      pushMutation(
        createReplaceNodeMutation(
          target,
          asDetachedNode(nodeToReplaceWith),
          options,
        ),
      );
    }: $FlowFixMe): TransformReplaceAPIs['replaceNode']),

    replaceStatementWithMany: ((
      target,
      nodesToReplaceWith,
      options?: ReplaceNodeOptions,
    ): void => {
      pushMutation(
        createReplaceStatementWithManyMutation(
          target,
          nodesToReplaceWith.map(n => asDetachedNode(n)),
          options,
        ),
      );
    }: TransformReplaceAPIs['replaceStatementWithMany']),
  };
  const modifyAPIs: TransformModifyAPIs = {
    modifyNodeInPlace: ((target: ESNode, newProps: $ReadOnly<{...}>): void => {
      const detachedProps = {};
      for (const [key, value] of Object.entries(newProps)) {
        if (isNode(value)) {
          // $FlowFixMe[incompatible-type]
          const node: ESNode = value;
          detachedProps[key] = asDetachedNode(node);
        } else {
          detachedProps[key] = value;
        }
      }

      pushMutation(createModifyNodeInPlaceMutation(target, detachedProps));
    }: TransformModifyAPIs['modifyNodeInPlace']),
  };

  return {
    mutations,

    // $FlowExpectedError[unsafe-getters-setters]
    get astWasMutated(): boolean {
      return mutations.length > 0;
    },

    ...cloneAPIs,
    ...commentAPIs,
    ...insertAPIs,
    ...modifyAPIs,
    ...removeAPIs,
    ...replaceAPIs,
  };
}

function toArray<T>(thing: SingleOrArray<T>): $ReadOnlyArray<T> {
  if (Array.isArray(thing)) {
    return (thing: $FlowFixMe);
  }
  return [thing];
}
