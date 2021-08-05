/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @emails oncall+flow
 * @format
 */

'use strict';

const {RuleTester: ESLintTester} = require('eslint');
const plugin = require('./index');

ESLintTester.setDefaultConfig({
  parser: require.resolve('hermes-eslint'),
  parserOptions: {
    ecmaVersion: 6,
    sourceType: 'module',
  },
});

function runTest(ruleName, testConfig) {
  const eslintTester = new ESLintTester();
  const rule = plugin.rules[ruleName];
  if (rule == null) {
    throw new Error(`Invalid rule name '${ruleName}'.`);
  }
  eslintTester.run(ruleName, rule, testConfig);
}

module.exports = runTest;
