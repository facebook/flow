/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

'use strict';

import type {VisitorKeysType} from '../traverse/getVisitorKeys';
import type {ESNode, Program} from 'flow-estree-oxidized';

import {SimpleTraverser} from '../traverse/SimpleTraverser';
import {
  nodeWith,
  removeNodeOnParent,
  replaceNodeOnParent,
} from './astNodeMutationHelpers';

/**
 * Transform callback
 * @param node The node we are visiting
 * @returns
 *   - return input node, signals no changes were made will continue to the next node.
 *   - return new node, the old node will be replaced in the AST. The new node and its
 *     children are then traversed.
 *   - return null, signals the node should be deleted from the AST.
 */
export type TransformCallback = (
  node: ESNode,
) => ESNode | $ReadOnlyArray<ESNode> | null;

export type TransformOptions = $ReadOnly<{
  /** The callback function which is called on entering each node. */
  transform: TransformCallback,

  /** The set of visitor keys to use for traversal. Defaults to the Flow ESTree visitor keys */
  visitorKeys?: ?VisitorKeysType,
}>;

function setParentPointer(
  node: ESNode | $ReadOnlyArray<ESNode>,
  parent: ?ESNode,
): void {
  if (parent != null) {
    if (Array.isArray(node)) {
      for (const item of node) {
        // $FlowExpectedError[cannot-write]
        item.parent = parent;
      }
    } else {
      // $FlowExpectedError[cannot-write]
      node.parent = parent;
    }
  }
}

/**
 * A simple class to recursively tranform AST trees.
 */
export class SimpleTransform {
  /**
   * Transform the given AST tree.
   * @param rootNode The root node to traverse.
   * @param options The option object.
   * @return The modified rootNode or a new node if the rootNode was transformed directly.
   */
  transform(rootNode: ESNode, options: TransformOptions): ESNode | null {
    let resultRootNode: ESNode | null = rootNode;
    SimpleTraverser.traverse(rootNode, {
      enter: (node: ESNode, parent: ?ESNode) => {
        // Ensure the parent pointers are correctly set before entering the node.
        setParentPointer(node, parent);

        const resultNode: ESNode | $ReadOnlyArray<ESNode> | null =
          options.transform(node);
        if (resultNode !== node) {
          let traversedResultNode: ESNode | $ReadOnlyArray<ESNode> | null =
            null;

          if (resultNode != null) {
            // Ensure the new node has the correct parent pointers before recursing again.
            setParentPointer(resultNode, parent);

            if (Array.isArray(resultNode)) {
              traversedResultNode = resultNode
                .map(item => this.transform(item, options))
                .filter(item => item != null);
            } else {
              traversedResultNode = this.transform(resultNode, options);
            }
          }

          if (parent == null) {
            if (node !== rootNode) {
              throw new Error(
                'SimpleTransform infra error: Parent not set on non root node, this should not be possible',
              );
            }
            if (Array.isArray(traversedResultNode)) {
              throw new Error(
                'SimpleTransform: invalid array result for root node',
              );
            } else {
              resultRootNode = traversedResultNode;
            }
          } else if (traversedResultNode == null) {
            removeNodeOnParent(node, parent, options.visitorKeys);
          } else {
            replaceNodeOnParent(
              node,
              parent,
              traversedResultNode,
              options.visitorKeys,
            );
            setParentPointer(traversedResultNode, parent);
          }

          throw SimpleTraverser.Skip;
        }
      },
      leave(_node: ESNode) {},
      visitorKeys: options.visitorKeys,
    });
    return resultRootNode;
  }

  /**
   * Transform the given AST tree.
   * @param node The root node to traverse.
   * @param options The option object.
   */
  static transform(node: ESNode, options: TransformOptions): ESNode | null {
    return new SimpleTransform().transform(node, options);
  }

  static transformProgram(
    program: Program,
    options: TransformOptions,
  ): Program {
    const result = SimpleTransform.transform(program, options);
    if (result?.type === 'Program') {
      return result;
    }
    throw new Error('SimpleTransform.transformProgram: Expected program node.');
  }

  /**
   * Return a new AST node with the given properties overrided if needed.
   *
   * This function takes care to only create new nodes when needed. Referential equality of nodes
   * is important as its used to know if a node should be re-traversed.
   *
   * @param node The base AST node.
   * @param overrideProps New properties to apply to the node.
   * @return Either the orginal node if the properties matched the existing node or a new node with
   *         the new properties.
   */
  static nodeWith<T: ESNode>(
    node: T,
    overrideProps: Partial<T>,
    visitorKeys?: VisitorKeysType,
  ): T {
    return nodeWith<T>(node, overrideProps, visitorKeys);
  }
}
