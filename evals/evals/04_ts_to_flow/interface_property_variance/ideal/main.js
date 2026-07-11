/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

// A minimal view of a running histogram. Consumers inspect the running count
// and max, and push new bucket indices in through `bucket`; they never read
// `bucket` back through this interface.
interface Histogram {
  readonly count: number;
  readonly max: number;
  writeonly bucket: number;
}

// The concrete aggregator accepts either a numeric bucket index or a named
// sentinel ("overflow", "underflow") from producers outside this file.
type Aggregator = {
  count: number,
  max: number,
  bucket: number | string,
};

function log(hist: Histogram, index: number): void {
  hist.bucket = index;
}

const agg: Aggregator = {count: 0, max: 0, bucket: 'overflow'};
const hist: Histogram = agg;

log(hist, 3);
log(hist, 7);

const c: number = hist.count;
const m: number = hist.max;
console.log(c, m);
