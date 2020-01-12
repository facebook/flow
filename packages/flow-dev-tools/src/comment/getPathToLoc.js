/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import type {FlowLoc} from '../flowResult';

export type PathNode = {
  ast: Object,
  key: string,
};

class Path {
  nodes: Array<{
    ast: Object;
    key: string;
    todo: Array<string>;
  }>;

  constructor(ast: Object) {
    this.nodes = [];
    this.push("root", ast, new Set(['comments']));
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
    while(this.nodes.length > 0) {
      const last = this.nodes[this.nodes.length-1];
      if (last.todo.length === 0) {
        this.nodes.pop();
        continue;
      }
      const prop = last.todo.pop();
      const ast = last.ast[prop];
      if (this.push(prop, ast, new Set(["range"]))) {
        return ast;
      }
    }
    return null;
  }

  getPath(): Array<PathNode> {
    return this.nodes.map(({ast, key}) => ({ast, key}));
  }
}

// Returns true if position a comes before or is equal to position b
function beforeOrEqual(
  a: {line: number, column: number},
  b: {line: number, column: number},
): boolean {
  if (a.line === b.line) {
    return a.column <= b.column;
  }
  return a.line <= b.line;
}

// Returns true IFF the ast location falls within the error loc
function errorLocMatchesAstLoc(errorLoc: FlowLoc, astLoc: Object): boolean {
  const errorLocFixedStart = {
    line: errorLoc.start.line,
    column: errorLoc.start.column - 1,
  }
  return beforeOrEqual(errorLocFixedStart, astLoc.start) &&
    beforeOrEqual(astLoc.end, errorLoc.end);
}

function rangeMatchesAstRange(range: [number, number], astRange: [number, number]): boolean {
  return range[0] == astRange[0] && range[1] == astRange[1];
}

/* Given a location and an AST, find the ast node whose location falls within
 * the given location. Then return the path to that node. */
export default function (errorLoc: FlowLoc, astRoot: Object): ?Array<PathNode> {
  const path = new Path(astRoot);
  let ast = path.next();
  while (ast != null) {
    if (ast.loc && errorLocMatchesAstLoc(errorLoc, ast.loc)) {
      return path.getPath();
    }
    ast = path.next();
  }

  return null;
}

export function getNodeAtRange (range: [number, number], astRoot: Object): ?Object {
  const path = new Path(astRoot);
  let ast = path.next();
  while (ast != null) {
    if (ast.range && rangeMatchesAstRange(range, ast.range)) {
      return ast
    }
    ast = path.next();
  }

  return null;
}
