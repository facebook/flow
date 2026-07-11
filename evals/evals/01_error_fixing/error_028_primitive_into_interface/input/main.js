/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

function totalPayloadBytes(lines: Iterable<string>): number {
  let bytes = 0;
  for (const line of lines) {
    bytes += line.length + 1;
  }
  return bytes;
}

const banner = 'GET /health HTTP/1.1';
console.log('payload =', totalPayloadBytes(banner));
