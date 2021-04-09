/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

// Must be before we require `../index`
const originalWeakMap = global.WeakMap;
global.WeakMap = undefined;

const Enum = require('../index');

global.WeakMap = originalWeakMap;

test('works when `WeakMap` is not defined', () => {
  const F = Enum({A: 1});
  expect(F.isValid(1)).toBe(true);
});
