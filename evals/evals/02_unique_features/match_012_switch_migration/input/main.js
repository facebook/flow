/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type LogLevel = 'debug' | 'info' | 'warn' | 'error' | 'fatal';

export function logLevelToNumber(level: LogLevel): number {
  switch (level) {
    case 'debug':
      return 0;
    case 'info':
      return 1;
    case 'warn':
      return 2;
    case 'error':
    case 'fatal':
      return 3;
  }
}
