/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

class MetricSample {
  name: string;
  value: number;
  constructor(name: string, value: number) {
    this.name = name;
    this.value = value;
  }
}

class CounterSnapshot {
  name: string;
  value: number;
  ts: number;
  constructor(name: string, value: number, ts: number) {
    this.name = name;
    this.value = value;
    this.ts = ts;
  }
}

function formatSample(s: interface {name: string, value: number}): string {
  const padded = s.name.padEnd(12, '.');
  const scaled = Math.round(s.value * 100) / 100;
  return `${padded}${scaled}`;
}

const cpu = new MetricSample('cpu', 0.734);
const mem = new CounterSnapshot('mem_rss', 512.4, Date.now());

console.log(formatSample(cpu));
console.log(formatSample(mem));
