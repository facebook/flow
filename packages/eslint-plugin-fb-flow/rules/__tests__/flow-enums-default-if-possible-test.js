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

const ESLintTester = require('../../eslint-tester');
const eslintTester = new ESLintTester();
const rule = require('../flow-enums-default-if-possible');

eslintTester.run('flow-enums-default-if-possible', rule, {
  valid: [
    'enum Status {}',
    'enum Status {Active, Off}',
    'enum Status {Active = "Active", Off = "off"}',
  ],
  invalid: [
    {
      code: `
enum Status {
  Active = "Active"
}`,
      errors: 1,
      output: `
enum Status {
  Active
}`,
    },
    {
      code: `
enum Status {
  Active = "Active",
}`,
      errors: 1,
      output: `
enum Status {
  Active,
}`,
    },
    {
      code: `
enum Status {
  Active = "Active",
  Off = "Off",
}`,
      errors: 1,
      output: `
enum Status {
  Active,
  Off,
}`,
    },
  ],
});
