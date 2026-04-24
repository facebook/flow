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
  ForInStatement,
  ForOfStatement,
  ModuleDeclaration,
  Statement,
  StatementParentArray,
  StatementParentSingle,
} from 'flow-estree-oxidized';

import {InvalidStatementError} from '../../Errors';

type StatementParent = $ReadOnly<
  | {
      type: 'single',
      parent: StatementParentSingle,
      key: string,
    }
  | {
      type: 'array',
      parent: StatementParentArray,
      key: string,
      targetIndex: number,
    },
>;

export function getStatementParent(
  target: ModuleDeclaration | Statement,
): StatementParent {
  function assertValidStatementLocation<T: $ReadOnly<interface {type: string}>>(
    parentWithType: T,
    ...invalidKeys: $ReadOnlyArray<$Keys<T>>
  ): void {
    for (const key of invalidKeys) {
      // $FlowExpectedError[prop-missing]
      const value = parentWithType[key];

      if (
        // $FlowFixMe[invalid-compare]
        value === target ||
        (Array.isArray(value) && value.includes(target))
      ) {
        throw new InvalidStatementError(
          `Attempted to insert a statement into \`${parentWithType.type}.${key}\`.`,
        );
      }
    }
  }
  function getAssertedIndex(key: string, arr: $ReadOnlyArray<mixed>): number {
    const idx = arr.indexOf(target);
    if (idx === -1) {
      throw new InvalidStatementError(
        `Could not find target in array of \`${parent.type}.${key}\`.`,
      );
    }
    return idx;
  }

  const parent = target.parent;
  const result: StatementParent = (() => {
    switch (parent.type) {
      case 'IfStatement': {
        assertValidStatementLocation(parent, 'test');
        const key = parent.consequent === target ? 'consequent' : 'alternate';
        return {type: 'single', parent, key};
      }

      case 'LabeledStatement': {
        assertValidStatementLocation(parent, 'label');
        return {type: 'single', parent, key: 'body'};
      }

      case 'WithStatement': {
        assertValidStatementLocation(parent, 'object');
        return {type: 'single', parent, key: 'body'};
      }

      case 'DoWhileStatement':
      case 'WhileStatement': {
        assertValidStatementLocation(parent, 'test');
        return {type: 'single', parent, key: 'body'};
      }

      case 'ForStatement': {
        assertValidStatementLocation(parent, 'init', 'test', 'update');
        return {type: 'single', parent, key: 'body'};
      }

      case 'ForInStatement':
      case 'ForOfStatement': {
        assertValidStatementLocation<ForInStatement | ForOfStatement>(
          parent,
          // $FlowExpectedError[prop-missing] - flow does not track properties from parent interface
          'left',
          // $FlowExpectedError[prop-missing] - flow does not track properties from parent interface
          'right',
        );
        return {type: 'single', parent, key: 'body'};
      }

      case 'SwitchCase': {
        assertValidStatementLocation(parent, 'test');
        return {
          type: 'array',
          parent,
          key: 'consequent',
          targetIndex: getAssertedIndex('consequent', parent.consequent),
        };
      }

      case 'BlockStatement':
      case 'Program': {
        return {
          type: 'array',
          parent,
          key: 'body',
          targetIndex: getAssertedIndex('body', parent.body),
        };
      }
    }

    throw new InvalidStatementError(
      `Expected to find a valid statement parent, but found a parent of type "${parent.type}".`,
    );
  })();

  if (
    // array insertions are already validated by the getAssertedIndex function
    result.targetIndex == null &&
    // $FlowExpectedError[prop-missing]
    // $FlowFixMe[invalid-compare]
    result.parent[result.key] !== target
  ) {
    throw new InvalidStatementError(
      `Expected to find the target "${target.type}" on the "${result.parent.type}.${result.key}", but found a different node. ` +
        'This likely means that you attempted to mutate around the target after it was deleted/replaced.',
    );
  }

  return result;
}
