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

import type {ParseForESLintOptions, ParseForESLintReturn} from '../../src';

import {parseForESLint as parse, ScopeType} from '../../src';

/**
 * Tests forked from eslint-scope expect a default sourceType of 'script', not 'module'.
 * Preserve this behavior to avoid updating all eslint-scope tests.
 */
function parseForESLint(
  code: string,
  options?: ParseForESLintOptions,
): ParseForESLintReturn {
  return parse(code, {
    ...options,
    sourceType: options?.sourceType ?? 'script',
  });
}

test('eslint-scope tests default to script sourceType', () => {
  const {ast, scopeManager} = parseForESLint('Foo');

  expect(ast.sourceType).toEqual('script');
  expect(scopeManager.scopes).toHaveLength(1);
  expect(scopeManager.scopes[0].type).toEqual(ScopeType.Global);
});

module.exports = {parseForESLint};
