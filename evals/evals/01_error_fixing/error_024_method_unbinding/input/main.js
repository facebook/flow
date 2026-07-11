/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

class Stopwatch {
  startedAt: number = 0;
  totalMs: number = 0;
  laps: number = 0;

  lap(nowMs: number): number {
    const elapsed = nowMs - this.startedAt;
    this.totalMs += elapsed;
    this.laps += 1;
    this.startedAt = nowMs;
    return elapsed;
  }
}

function runTicks(
  onTick: (nowMs: number) => number,
  ticks: ReadonlyArray<number>,
): number {
  let last = 0;
  for (const t of ticks) {
    last = onTick(t);
  }
  return last;
}

const sw: Stopwatch = new Stopwatch();
sw.startedAt = 100;
const tick = sw.lap;
const lastElapsed: number = runTicks(tick, [150, 220, 260]);
console.log(`last lap ${lastElapsed}ms, ${sw.laps} laps`);
