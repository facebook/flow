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

runTest('no-flow-enums-object-mapping', {
  valid: [
    'const o = {A: 1, B: 2};',
    'const o = {[Foo.A]: 1, [Foo.B]: 2};',
    'const o = {[(Foo[x]: string)]: 1};',
    'const o = {[Foo.B]: 2};',
    'const o = {[(x: string)]: 1};',
    'const o = {[(String(x))]: 2};',
    'const o = {[(Foo.A: T)]: 1};',
    'const o = {[f(Foo.A)]: 1};',
    'const o = {[(Foo.A: string)]: 1, ...Bar};',
    'const o = {};',
  ],
  invalid: [
    {
      code: 'const o = {[(Foo.A: string)]: 1, [(Foo.B: string)]: 2};',
      errors: [
        {
          messageId: 'message',
        },
      ],
    },
    {
      code: 'const o = {[String(Foo.A)]: 1, [String(Foo.B)]: 2};',
      errors: [
        {
          messageId: 'message',
        },
      ],
    },
  ],
});
