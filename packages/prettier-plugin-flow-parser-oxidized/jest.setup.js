/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

function failing(name, fn, timeout) {
  return test(
    name,
    async () => {
      let failed = false;
      try {
        await fn();
      } catch (_error) {
        failed = true;
      }

      if (!failed) {
        throw new Error('Expected test to fail, but it passed.');
      }
    },
    timeout,
  );
}

if (typeof test.failing !== 'function') {
  test.failing = failing;
}

if (typeof it.failing !== 'function') {
  it.failing = failing;
}
