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

import type {
  ArrowFunctionExpression,
  BinaryExpression,
  BindingName,
  CallExpression,
  ESNode,
  Expression,
  Identifier,
  LogicalExpression,
  NullLiteral,
  NumericLiteral,
  Position,
  Range,
  SourceLocation,
  SpreadElement,
  Statement,
  StringLiteral,
  Super,
  ThrowStatement,
  VariableDeclaration,
} from 'flow-estree-oxidized';

// Rely on the mapper to fix up parent relationships.
export const EMPTY_PARENT: $FlowFixMe = null;

export function createDefaultPosition(): Position {
  return {
    line: 1,
    column: 0,
  };
}

export type Etc = {
  parent?: ESNode,
  loc?: SourceLocation,
  range?: Range,
};

export function etc({loc, range, parent}: Etc = {}): {
  parent: ESNode,
  loc: SourceLocation,
  range: Range,
} {
  return {
    loc: {
      start: loc?.start != null ? loc.start : createDefaultPosition(),
      end: loc?.end != null ? loc.end : createDefaultPosition(),
    },
    range: range ?? [0, 0],
    parent: parent ?? EMPTY_PARENT,
  };
}

export function ident(name: string, info?: Etc): Identifier {
  return {
    type: 'Identifier',
    name,
    optional: false,
    typeAnnotation: null,
    ...etc(info),
  };
}

export function stringLiteral(value: string, info?: Etc): StringLiteral {
  return {
    type: 'Literal',
    value,
    raw: `"${value}"`,
    literalType: 'string',
    ...etc(info),
  };
}

export function numberLiteral(value: number, info?: Etc): NumericLiteral {
  return {
    type: 'Literal',
    value,
    raw: String(value),
    literalType: 'numeric',
    ...etc(info),
  };
}

export function nullLiteral(info?: Etc): NullLiteral {
  return {
    type: 'Literal',
    value: null,
    raw: 'null',
    literalType: 'null',
    ...etc(info),
  };
}

export function conjunction(tests: $ReadOnlyArray<Expression>): Expression {
  if (tests.length === 0) {
    throw new Error('Must have at least one test.');
  }
  return tests.reduce((acc, test): LogicalExpression => ({
    type: 'LogicalExpression',
    left: acc,
    right: test,
    operator: '&&',
    ...etc(),
  }));
}

export function disjunction(tests: $ReadOnlyArray<Expression>): Expression {
  if (tests.length === 0) {
    throw new Error('Must have at least one test.');
  }
  return tests.reduce((acc, test): LogicalExpression => ({
    type: 'LogicalExpression',
    left: acc,
    right: test,
    operator: '||',
    ...etc(),
  }));
}

export function variableDeclaration(
  kind: VariableDeclaration['kind'],
  id: BindingName,
  init: Expression,
  info?: Etc,
): VariableDeclaration {
  return {
    type: 'VariableDeclaration',
    kind,
    declarations: [
      {
        type: 'VariableDeclarator',
        init,
        id,
        ...etc(),
        parent: EMPTY_PARENT,
      },
    ],
    ...etc(info),
  };
}

export function callExpression(
  callee: Expression | Super,
  args: $ReadOnlyArray<Expression | SpreadElement>,
  info?: Etc,
): CallExpression {
  return {
    type: 'CallExpression',
    callee,
    arguments: args,
    typeArguments: null,
    optional: false,
    ...etc(info),
  };
}

export function throwStatement(arg: Expression, info?: Etc): ThrowStatement {
  return {
    type: 'ThrowStatement',
    argument: callExpression(ident('Error'), [arg]),
    ...etc(info),
  };
}

export function iife(
  statements: $ReadOnlyArray<Statement>,
  params: $ReadOnlyArray<BindingName> = [],
  args: $ReadOnlyArray<Expression> = [],
): CallExpression {
  const callee: ArrowFunctionExpression = {
    type: 'ArrowFunctionExpression',
    params,
    expression: false,
    async: false,
    predicate: null,
    returnType: null,
    typeParameters: null,
    id: null,
    body: {
      type: 'BlockStatement',
      body: statements,
      ...etc(),
    },
    ...etc(),
  };
  return callExpression(callee, args);
}

export function typeofExpression(
  arg: Expression,
  kind: string,
): BinaryExpression {
  return {
    type: 'BinaryExpression',
    left: {
      type: 'UnaryExpression',
      operator: 'typeof',
      argument: arg,
      prefix: true,
      ...etc(),
    },
    right: stringLiteral(kind),
    operator: '===',
    ...etc(),
  };
}
