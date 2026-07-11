/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

interface LogRecord {
  level: string;
  message: string;
  timestamp: number;
}

class TimedLogRecord {
  level: string;
  message: string;
  timestamp: number;

  constructor(level: string, message: string) {
    this.level = level;
    this.message = message;
    this.timestamp = Date.now();
  }
}

function formatEntry(entry: {
  level: string,
  message: string,
  timestamp: number,
}): string {
  const tag = entry.level.toUpperCase();
  const seconds = Math.floor(entry.timestamp / 1000);
  return `[${seconds}] ${tag}: ${entry.message}`;
}

declare const fromProvider: LogRecord;

const built: TimedLogRecord = new TimedLogRecord('warn', 'disk almost full');
const literal = {level: 'info', message: 'starting up', timestamp: 0};

console.log(formatEntry(built));
console.log(formatEntry(fromProvider));
console.log(formatEntry(literal));
