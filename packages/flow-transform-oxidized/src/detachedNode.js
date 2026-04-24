/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {BaseNode, ESNode} from 'flow-estree-oxidized';

import {astNodeMutationHelpers} from 'flow-parser-oxidized';

export opaque type DetachedNode<+T> = T;
export type MaybeDetachedNode<+T> = T | DetachedNode<T>;

type DetachConfig = $ReadOnly<{
  preserveLocation?: boolean,
  originalNode?: ESNode,
}>;

const DETACHED_MARKER = Symbol.for('hermes-transform - Detached AST Node');
const ORIGINAL_NODE = Symbol.for('hermes-transform - Original Node');

export function isDetachedNode(node: MaybeDetachedNode<ESNode>): boolean {
  // $FlowExpectedError[invalid-in-lhs] flow doesn't support symbols as keys
  return DETACHED_MARKER in node;
}
export function getOriginalNode(node: MaybeDetachedNode<ESNode>): ?ESNode {
  // $FlowExpectedError[prop-missing]
  return node[ORIGINAL_NODE];
}

/* $FlowExpectedError[unclear-type] Type safety is not needed for generated
 * code, this avoids us always having to pass a generic property within codegen */
export function asDetachedNodeForCodeGen(node: any): ?DetachedNode<any> {
  if (node == null) {
    return null;
  }

  if (isDetachedNode(node)) {
    return node;
  }

  return shallowCloneNode<ESNode>(node, {});
}

export const asDetachedNode: {
  <T: ESNode>(
    node: MaybeDetachedNode<T>,
    config?: {useDeepClone: boolean},
  ): DetachedNode<T>,
  <T: ?ESNode>(
    node: ?MaybeDetachedNode<T>,
    config?: {useDeepClone: boolean},
  ): ?DetachedNode<T>,
  // $FlowFixMe[incompatible-exact]
} = <T: ESNode>(
  node: ?MaybeDetachedNode<T>,
  {useDeepClone}: {useDeepClone: boolean} = {useDeepClone: false},
): // $FlowExpectedError[incompatible-type]
?DetachedNode<T> => {
  if (node == null) {
    return null;
  }

  if (isDetachedNode((node: MaybeDetachedNode<T>))) {
    return node;
  }

  return useDeepClone
    ? deepCloneNode<T>(node, {})
    : shallowCloneNode<T>(node, {});
};

// used by the node type function codegen
export function detachedProps<T: BaseNode>(
  parent: ?ESNode,
  props: {...},
  config: DetachConfig = {},
): DetachedNode<T> {
  // $FlowExpectedError[incompatible-type]
  const detachedNode: DetachedNode<T> = {
    ...props,
    ...(config?.preserveLocation !== true
      ? {
          // if this is [0,0] AND the file has a docblock then prettier will insert newlines between
          // certain detached nodes. Because of "intended" formatting behaviour (https://github.com/prettier/prettier/issues/12078)
          // this can cause us to output weirdly formatted code that should have been collapsed.
          //
          // prettier works backwards from the position you give it to find newlines or non whitespace
          // tokens and uses this to determine if newlines should be inserted between nodes.
          // By placing the range at [1, 1] we can ensure a token is always found before a newline
          // and therefore no newlines will be placed between nodes.
          //
          // NOTE: we will still run into the bug if there is weird code like a docblock with whitespace
          //       characters before it. However we assume this isn't going to happen because any file
          //       already formatted by prettier will have that whitespace removed.
          // We considered a more complex solution involving traversing the AST and manually updating
          // detached node ranges after mutations are applied - however this is a lot heavier and will
          // probably never be needed.
          range: [1, 1],
          loc: {
            start: {
              line: 1,
              column: 0,
            },
            end: {
              line: 1,
              column: 0,
            },
          },
        }
      : {}),
    // if not provided, then we purposely don't set this here
    // and will rely on the tooling to update it as appropriate.
    // nothing should be reading from this before it's set anyway.
    parent: (parent: $FlowFixMe),
  };

  // mark the node as detached
  Object.defineProperty(detachedNode, DETACHED_MARKER, {
    configurable: false,
    enumerable: false,
    value: true,
    writable: false,
  });

  if (config.originalNode) {
    Object.defineProperty(detachedNode, ORIGINAL_NODE, {
      configurable: false,
      enumerable: false,
      value: config.originalNode,
      writable: false,
    });
  }

  return detachedNode;
}

/**
 * Shallowly clones the node, but not its children.
 */
export function shallowCloneNode<T: ESNode>(
  node: T,
  newProps: {...},
  config?: DetachConfig = {},
): DetachedNode<T> {
  return detachedProps<T>(
    null,
    {...(node: $FlowFixMe), ...newProps},
    {
      preserveLocation: config.preserveLocation ?? true,
      originalNode: config.originalNode ?? node,
    },
  );
}

/**
 * Deeply clones node and its entire tree.
 */
export function deepCloneNode<T: ESNode>(
  node: T,
  newProps: {...},
): DetachedNode<T> {
  // $FlowFixMe[unsafe-object-assign]
  const clone: DetachedNode<T> = Object.assign(
    JSON.parse(
      JSON.stringify(node, (key, value) => {
        // null out parent pointers
        if (key === 'parent') {
          return undefined;
        }
        return value;
      }),
    ),
    newProps,
  );

  updateAllParentPointers(clone);

  // $FlowExpectedError[class-object-subtyping]
  return detachedProps<T>(null, clone);
}

/**
 * Corrects the parent pointers in direct children of the given node
 */
export function setParentPointersInDirectChildren(
  node: DetachedNode<ESNode>,
): void {
  astNodeMutationHelpers.setParentPointersInDirectChildren(node);
}

/**
 * Traverses the entire subtree to ensure the parent pointers are set correctly
 */
export function updateAllParentPointers(
  node: ESNode | DetachedNode<ESNode>,
): void {
  astNodeMutationHelpers.updateAllParentPointers(node);
}
