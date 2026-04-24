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

import type {Program} from 'flow-estree-oxidized';
import type {ScopeManager} from 'flow-eslint-oxidized';

import {attachComments} from './comments/comments';
import {parseForESLint} from 'flow-eslint-oxidized';

export type ParseResult = {
  ast: Program,
  scopeManager: ScopeManager,
  code: string,
};

export function parse(code: string): ParseResult {
  const {ast, scopeManager} = parseForESLint(code, {
    sourceType: 'module',
    enableExperimentalComponentSyntax: true,
  });

  // Don't include the docblock comment in the comment list as we don't want to attach it
  // as it should be maintained at the top of the file as nodes are moved around.
  let comments = ast.comments;
  if (ast.docblock != null && ast.docblock.comment === comments[0]) {
    const [first, ...nonDocblockComments] = comments;

    // Since we will not be attaching this comment automatically we need to add the
    // properties prettier expects for printing.
    // $FlowExpectedError[prop-missing]
    first.placement = 'endOfLine';
    // $FlowExpectedError[prop-missing]
    first.leading = true;
    // $FlowExpectedError[prop-missing]
    first.trailing = false;
    // $FlowExpectedError[prop-missing]
    first.printed = false;

    comments = nonDocblockComments;
  }

  // attach comments before mutation. this will ensure that as nodes are
  // cloned / moved around - comments remain in the correct place with respect to the node
  attachComments(comments, ast, code);

  return {ast, scopeManager, code};
}
