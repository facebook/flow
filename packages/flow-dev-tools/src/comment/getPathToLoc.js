/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import type {FlowLoc} from '../flowResult';

type ASTNode = Object;

export type PathNode = {
  ast: ASTNode,
  key: string,
};

const EXCLUDE_NODES_FROM_TRAVERSAL = new Set(['range', 'comments', 'loc']);

function getChildNodeContainingLocation(
  node: ASTNode,
  isLocationWithinNode: (node: ASTNode) => boolean,
): null | Array<PathNode> {
  // Assuming node siblings are non overlapping we only need to find the
  // first node that has contains the location.
  for (const [key, childNode] of Object.entries(node)) {
    if (Array.isArray(childNode)) {
      // Assume list items only contain ASTNode's (no nested lists).
      for (const [listKey, listChildNode] of Object.entries(childNode)) {
        if (isLocationWithinNode(listChildNode)) {
          return [
            {key, ast: childNode},
            {key: listKey, ast: listChildNode},
          ];
        }
      }
      continue;
    }

    if (
      !EXCLUDE_NODES_FROM_TRAVERSAL.has(key) &&
      isLocationWithinNode(childNode)
    ) {
      return [{key, ast: childNode}];
    }
  }

  return null;
}

/**
 * Find path to a location within an AST.
 *
 * Note: This function assumes children nodes are always equal to or within
 *       the location of its parent and no siblings are overlapping. This
 *       allows much faster traversal but will fail to find nodes if these
 *       constraints are not maintained.
 */
function findPathToLocation(
  rootNode: ASTNode,
  filterNodeByLocation: (node: ASTNode) => boolean,
): Array<PathNode> {
  const path: Array<PathNode> = [{key: 'root', ast: rootNode}];

  const isLocationWithinNode = (node: ASTNode) =>
    node != null &&
    node.type != null &&
    node.loc != null &&
    filterNodeByLocation(node);

  // Search until we no longer match
  while (true) {
    const foundPathNodes = getChildNodeContainingLocation(
      path[path.length - 1].ast,
      isLocationWithinNode,
    );
    if (foundPathNodes == null) {
      return path;
    }

    path.push(...foundPathNodes);
  }

  return path;
}

type Position = {line: number, column: number};

// Returns true if position a comes before or is equal to position b
function isBeforeOrEqual(a: Position, b: Position): boolean {
  if (a.line === b.line) {
    return a.column <= b.column;
  }
  return a.line <= b.line;
}

/* Given a location and an AST, find the ast node whose location falls within
 * the given location. Then return the path to that node. */
function getPathToLoc(errorLoc: FlowLoc, astRoot: ASTNode): ?Array<PathNode> {
  return findPathToLocation(
    astRoot,
    node =>
      isBeforeOrEqual(node.loc.start, errorLoc.start) &&
      isBeforeOrEqual(errorLoc.end, node.loc.end),
  );
}

function getNodeAtRange(range: [number, number], astRoot: ASTNode): ?ASTNode {
  const path = findPathToLocation(
    astRoot,
    node => node.range[0] <= range[0] && range[1] <= node.range[1],
  );

  const lastPathItem = path.pop();
  if (
    lastPathItem != null &&
    // We only care about exact matches
    lastPathItem.ast.range[0] === range[0] &&
    lastPathItem.ast.range[1] === range[1]
  ) {
    return lastPathItem.ast;
  }

  return null;
}

module.exports = {
  getNodeAtRange,
  default: getPathToLoc,
};
