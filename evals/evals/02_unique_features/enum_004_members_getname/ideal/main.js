/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export enum LogLevel of number {
  Debug = 0,
  Info = 1,
  Warn = 2,
  Error = 3,
  Fatal = 4,
}

export function allLevelNames(): Array<string> {
  return Array.from(LogLevel.members(), level => LogLevel.getName(level));
}

export function levelRange(min: LogLevel, max: LogLevel): Array<LogLevel> {
  const minVal: number = min as number;
  const maxVal: number = max as number;
  const result: Array<LogLevel> = [];
  for (const level of LogLevel.members()) {
    const val: number = level as number;
    if (val >= minVal && val <= maxVal) {
      result.push(level);
    }
  }
  return result;
}

export function formatLevel(level: LogLevel): string {
  const name = LogLevel.getName(level).toUpperCase();
  const value: number = level as number;
  return `${name} (${value})`;
}
