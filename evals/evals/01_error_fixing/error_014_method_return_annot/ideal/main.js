/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

class Counter {
  count: number;
  constructor(initial: number) {
    this.count = initial;
  }
  increment(by: number): number {
    this.count += by;
    return this.count;
  }
}

const c = new Counter(0);
const next: number = c.increment(5);
