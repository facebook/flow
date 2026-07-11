/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export opaque type Counter = {kind: 'counter', name: string, value: number};
export type Gauge = {kind: 'gauge', name: string, value: number, unit: string};
export type Metric = Counter | Gauge;

export function makeCounter(name: string, value: number): Counter {
  return {kind: 'counter', name, value};
}

export function isCounter(metric: Metric): metric is Counter {
  return metric.kind === 'counter';
}

export function counterName(metric: Counter): string {
  return metric.name;
}

export function counterValue(metric: Counter): number {
  return metric.value;
}
