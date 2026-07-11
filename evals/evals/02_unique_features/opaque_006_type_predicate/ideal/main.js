/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import {isCounter, counterName, counterValue, type Metric} from './Metric';

export function formatMetric(metric: Metric): string {
  if (isCounter(metric)) {
    return `counter:${counterName(metric)}=${counterValue(metric)}`;
  }
  return `gauge:${metric.name}=${metric.value}${metric.unit}`;
}

export function sumCounters(metrics: ReadonlyArray<Metric>): number {
  let total = 0;
  for (const metric of metrics) {
    if (isCounter(metric)) {
      total += counterValue(metric);
    }
  }
  return total;
}
