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
  return match (level) {
    'debug' => 0,
    'info' => 1,
    'warn' => 2,
    'error' | 'fatal' => 3,
  };
}
