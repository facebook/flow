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

export type PathNode = {
  ast: Object,
  key: string,
};

class Path {
  nodes: Array<{
    ast: Object,
    key: string,
    todo: Array<string>,
  }>;

  constructor(ast: Object) {
    this.nodes = [];
    this.push('root', ast, new Set(['comments']));
  }

  push(key: string, ast: Object, exclude?: Set<string>) {
    const todo = [];
    if (ast == null) {
      return false;
    } else if (Array.isArray(ast)) {
      for (let i = 0; i < ast.length; i++) {
        todo.push(String(i));
      }
    } else if (ast.type != null) {
      for (const prop in ast) {
        if (ast.hasOwnProperty(prop) && !(exclude && exclude.has(prop))) {
          todo.push(prop);
        }
      }
    } else {
      return false;
    }
    this.nodes.push({key, ast, todo});
    return true;
  }

  next() {
    while (this.nodes.length > 0) {
      const last = this.nodes[this.nodes.length - 1];
      if (last.todo.length === 0) {
        this.nodes.pop();
        continue;
      }
      const prop = last.todo.pop();
      const ast = last.ast[prop];
      if (this.push(prop, ast, new Set(['range']))) {
        return ast;
      }
    }
    return null;
  }

  getPath(): Array<PathNode> {
    return this.nodes.map(({ast, key}) => ({ast, key}));
  }
}

type Position = {line: number, column: number};

// Returns true if position a comes before or is equal to position b
function beforeOrEqual(a: Position, b: Position): boolean {
  if (a.line === b.line) {
    return a.column <= b.column;
  }
  return a.line <= b.line;
}

function beforePosition(position: Position): Position {
  return {
    line: position.line,
    column: position.column - 1,
  };
}

function afterPosition(position: Position): Position {
  return {
    line: position.line,
    column: position.column + 1,
  };
}

/**
 * Check if a AST node location falls within the error loc.
 */
function errorLocMatchesAstLoc(errorLoc: FlowLoc, ast: Object): boolean {
  const astLoc = ast.loc;

  // For JSXText nodes return true if the error loc falls within the ast
  // loc, as JSXText errors may point to only a portion of a JSXText node.
  if (
    ast.type === 'JSXText' &&
    beforeOrEqual(astLoc.start, errorLoc.start) &&
    beforeOrEqual(errorLoc.end, astLoc.end)
  ) {
    return true;
  }

  // The Flow OCaml AST has a node which represents the params, estree does
  // not have the equivalent node, this means Flow can produce error locations
  // which cannot be matched from JS. Typically this is not a problem since you
  // can match on the params themselves but for cases where the function has no
  // params we need to check if the error could be pointing to the params location.
  if (
    (ast.type === 'FunctionDeclaration' ||
      ast.type === 'FunctionExpression' ||
      ast.type === 'ArrowFunctionExpression') &&
    ast.params.length === 0
  ) {
    // Find the locations closest to the params to ensure we don't unnecessarily
    // match nodes.
    const startLoc =
      ast.typeParameters != null
        ? afterPosition(ast.typeParameters.loc.end)
        : ast.id != null
        ? afterPosition(ast.id.loc.end)
        : ast.loc.start;
    const endLoc =
      ast.returnType != null ? ast.returnType.loc.start : ast.body.loc.start;

    if (
      beforeOrEqual(startLoc, errorLoc.start) &&
      beforeOrEqual(errorLoc.end, endLoc)
    ) {
      return true;
    }
  }

  return (
    beforeOrEqual(beforePosition(errorLoc.start), astLoc.start) &&
    beforeOrEqual(astLoc.end, errorLoc.end)
  );
}

function rangeMatchesAstRange(
  range: [number, number],
  astRange: [number, number],
): boolean {
  return range[0] == astRange[0] && range[1] == astRange[1];
}

/* Given a location and an AST, find the ast node whose location falls within
 * the given location. Then return the path to that node. */
function getPathToLoc(errorLoc: FlowLoc, astRoot: Object): ?Array<PathNode> {
  const path = new Path(astRoot);
  let ast = path.next();
  while (ast != null) {
    if (ast.loc && errorLocMatchesAstLoc(errorLoc, ast)) {
      return path.getPath();
    }
    ast = path.next();
  }

  return null;
}

function getNodeAtRange(range: [number, number], astRoot: Object): ?Object {
  const path = new Path(astRoot);
  let ast = path.next();
  while (ast != null) {
    if (ast.range && rangeMatchesAstRange(range, ast.range)) {
      return ast;
    }
    ast = path.next();
  }

  return null;
}

module.exports = {
  getNodeAtRange,
  default: getPathToLoc,
};
