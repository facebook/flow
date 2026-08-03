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

import type {SourceLocation} from 'flow-estree';

import * as TSESTree from './ts-estree-ast-types';

const DUMMY_LOC: SourceLocation = {
  start: {line: 1, column: 0},
  end: {line: 1, column: 0},
};

/**
 * Extract statically known property key names from a list of TS type element
 * nodes and return them as `TSLiteralType` (string literal) AST nodes.
 *
 * Returns `null` if any member has a computed or non-extractable key,
 * or if the list is empty.
 */
export function extractPropertyKeyLiterals(
  members: ReadonlyArray<TSESTree.TypeElement>,
): Array<TSESTree.TSLiteralType> | null {
  // An empty member list has no keys to extract. Returning `null` here lets
  // the caller fall back to `keyof {}`, preserving the original output for
  // spread-only objects (e.g. `{...T1}`) where a literal union would be
  // invalid TS.
  if (members.length === 0) {
    return null;
  }
  const literals: Array<TSESTree.TSLiteralType> = [];
  for (const member of members) {
    if (
      member.type !== 'TSPropertySignature' &&
      member.type !== 'TSMethodSignature'
    ) {
      return null;
    }
    if (member.computed === true) {
      return null;
    }
    const {key} = member;
    if (key.type === 'Identifier') {
      literals.push({
        type: 'TSLiteralType',
        loc: DUMMY_LOC,
        literal: {
          type: 'Literal',
          loc: DUMMY_LOC,
          value: key.name,
          raw: `'${key.name}'`,
        } as TSESTree.StringLiteral,
      });
    } else if (key.type === 'Literal') {
      // A string or numeric literal key is already a valid literal type.
      literals.push({
        type: 'TSLiteralType',
        loc: DUMMY_LOC,
        literal: key,
      });
    } else {
      return null;
    }
  }
  return literals;
}

/**
 * Returns a type that is guaranteed to include `undefined`, without duplicating
 * it when already present.
 *
 * Flow's optional members permit `undefined`, so the faithful TypeScript
 * translation of `foo?: T` is `foo?: T | undefined` (the two are distinct under
 * `exactOptionalPropertyTypes`).
 */
export function ensureTypeIncludesUndefined(
  typeAnnotation: TSESTree.TypeNode,
): TSESTree.TypeNode {
  if (typeAnnotation.type === 'TSUndefinedKeyword') {
    return typeAnnotation;
  }
  if (typeAnnotation.type === 'TSUnionType') {
    if (typeAnnotation.types.some(type => type.type === 'TSUndefinedKeyword')) {
      return typeAnnotation;
    }
    return {
      type: 'TSUnionType',
      loc: DUMMY_LOC,
      types: [
        ...typeAnnotation.types,
        {type: 'TSUndefinedKeyword', loc: DUMMY_LOC},
      ],
    };
  }
  return {
    type: 'TSUnionType',
    loc: DUMMY_LOC,
    types: [typeAnnotation, {type: 'TSUndefinedKeyword', loc: DUMMY_LOC}],
  };
}
