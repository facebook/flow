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

const runTest = require('../../run-test');

runTest('use-flow-enums', {
  valid: [
    'enum Status {}',
    'enum Status {Active, Off}',
    'enum Status {Active = "Active", Off = "off"}',
    'enum Status {Active = 1, Off = 3}',
    'keyMirror({lowerCaseName: null});',
    'keyMirror({DuplicateName: null, DuplicateName: null});',
    'Object.freeze({A: "value with spaces"});',
    'Object.freeze({...others, A: "value with spaces"});',
    'Object.freeze({ComputedValue: 1 + 2});',
    'Object.freeze({A: "duplicateValue", B: "duplicateValue"});',
    'Object.freeze({Different: null, Typed: "foo", Values: 1})',
  ],
  invalid: [
    {
      code: 'Object.freeze({A: 1, B: 2});',
      errors: [
        {
          messageId: 'useFlowEnumsObjectFreeze',
          data: {
            key: 'A',
            value: '1',
          },
        },
      ],
    },
    {
      code: 'keyMirror({A: null, B: null});',
      errors: [
        {
          messageId: 'useFlowEnumsKeyMirror',
          data: {
            key: 'A',
          },
        },
      ],
    },
  ],
});
