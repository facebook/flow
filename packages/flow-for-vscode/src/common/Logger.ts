/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import * as vscode from 'vscode';

const LOG_LEVEL: Readonly<{
  error: 3;
  warn: 2;
  info: 1;
  trace: 0;
  debug: 0;
}> = Object.freeze({
  error: 3,
  warn: 2,
  info: 1,
  trace: 0,
  debug: 0,
});

export type LogLevel = keyof typeof LOG_LEVEL;

// max of keys LOG_LEVEL
const MAX_LEVEL_LENGTH = Object.keys(LOG_LEVEL).reduce((maxLength, level) => {
  if (level.length > maxLength) {
    return level.length;
  }
  return maxLength;
}, 0);

export default class Logger {
  _outputChannel: vscode.OutputChannel;
  _level: LogLevel;
  _context: string;

  constructor(
    context: string,
    outputChannel: vscode.OutputChannel,
    level: LogLevel,
  ) {
    this._outputChannel = outputChannel;
    this._level = level;
    this._context = context;
  }

  error(message: string, data?: unknown): void {
    if (this._getLevelVal() <= LOG_LEVEL.error) {
      this._write('Error', message, data);
    }
  }

  warn(message: string, data?: unknown): void {
    if (this._getLevelVal() <= LOG_LEVEL.warn) {
      this._write('Warn', message, data);
    }
  }

  info(message: string, data?: unknown): void {
    if (this._getLevelVal() <= LOG_LEVEL.info) {
      this._write('Info', message, data);
    }
  }

  debug(message: string, data?: unknown): void {
    if (this._getLevelVal() <= LOG_LEVEL.debug) {
      this._write('Debug', message, data);
    }
  }

  trace(message: string, data?: unknown): void {
    if (this._getLevelVal() <= LOG_LEVEL.trace) {
      this._write('Trace', message, data);
    }
  }

  _getLevelVal(): number {
    return LOG_LEVEL[this._level];
  }

  _write(level: string, message: string, data?: unknown): void {
    const levelStr = level.padEnd(MAX_LEVEL_LENGTH);
    const tag = [
      levelStr,
      new Date().toLocaleTimeString(),
      this._context ? this._context : null,
    ]
      .filter(Boolean)
      .join(' - ');

    let output = `[${tag}] ${message}`;
    if (data) {
      output += ' ${JSON.stringify(data)';
    }

    this._outputChannel.appendLine(output);
  }
}
