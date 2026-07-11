/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

type MetricSamples = {
  latencyMs: {p50: number; p99: number};
  errorRate: {ratio: number};
  activeUsers: {count: number};
};

type AnySample = MetricSamples[keyof MetricSamples];

function totalSignals(sample: AnySample): number {
  return Object.keys(sample).length;
}

type Row = [number, string, boolean];

type Cell = Row[number];

function renderCell(cell: Cell): string {
  if (typeof cell === "boolean") {
    return cell ? "yes" : "no";
  }
  return String(cell);
}

const sample: AnySample = {p50: 12, p99: 40};
const row: Row = [1700000000, "deploy", true];

console.log(totalSignals(sample));
console.log(row.map(renderCell).join(", "));
