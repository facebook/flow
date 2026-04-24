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

import type {VisitorKeysType} from './getVisitorKeys';
import type {ESNode} from 'flow-estree-oxidized';

import {getVisitorKeys, isNode} from './getVisitorKeys';

export type TraverserCallback = (node: ESNode, parent: ?ESNode) => void;
export type TraverserOptions = $ReadOnly<{
  /** The callback function which is called on entering each node. */
  enter: TraverserCallback,
  /** The callback function which is called on leaving each node. */
  leave: TraverserCallback,
  /** The set of visitor keys to use for traversal. Defaults to the Flow ESTree visitor keys */
  visitorKeys?: ?VisitorKeysType,
}>;

/**
 * Can be thrown within the traversal "enter" function to prevent the traverser
 * from traversing the node any further, essentially culling the remainder of the
 * AST branch
 */
export const SimpleTraverserSkip: Error = new Error();
/**
 * Can be thrown at any point during the traversal to immediately stop traversal
 * entirely.
 */
export const SimpleTraverserBreak: Error = new Error();

/**
 * A very simple traverser class to traverse AST trees.
 */
export class SimpleTraverser {
  static Break: Error = SimpleTraverserBreak;
  static Skip: Error = SimpleTraverserSkip;

  /**
   * Traverse the given AST tree.
   * @param node The root node to traverse.
   * @param options The option object.
   */
  traverse(node: ESNode, options: TraverserOptions): void {
    try {
      this._traverse(node, null, options);
    } catch (ex) {
      if (ex === SimpleTraverserBreak) {
        return;
      }
      throw ex;
    }
  }

  /**
   * Traverse the given AST tree recursively.
   * @param node The current node.
   * @param parent The parent node.
   * @private
   */
  _traverse(node: ESNode, parent: ?ESNode, options: TraverserOptions): void {
    if (!isNode(node)) {
      return;
    }

    try {
      options.enter(node, parent);
    } catch (ex) {
      if (ex === SimpleTraverserSkip) {
        return;
      }
      this._setErrorContext(ex, node);
      throw ex;
    }

    const keys = getVisitorKeys(node, options.visitorKeys);
    for (const key of keys) {
      const child: ESNode | $ReadOnlyArray<ESNode> = (node[
        (key: $FlowFixMe)
      ]: $FlowFixMe);

      if (Array.isArray(child)) {
        for (let j = 0; j < child.length; ++j) {
          this._traverse(child[j], node, options);
        }
      } else {
        this._traverse(child, node, options);
      }
    }

    try {
      options.leave(node, parent);
    } catch (ex) {
      if (ex === SimpleTraverserSkip) {
        return;
      }
      this._setErrorContext(ex, node);
      throw ex;
    }
  }

  /**
   * Set useful contextual information onto the error object.
   * @param ex The error object.
   * @param node The current node.
   * @private
   */
  _setErrorContext(ex: Error, node: ESNode): void {
    // $FlowFixMe[prop-missing]
    ex.currentNode = {
      type: node.type,
      range: node.range,
      loc: node.loc,
    };
  }

  /**
   * Traverse the given AST tree.
   * @param node The root node to traverse.
   * @param options The option object.
   */
  static traverse(node: ESNode, options: TraverserOptions) {
    new SimpleTraverser().traverse(node, options);
  }
}
