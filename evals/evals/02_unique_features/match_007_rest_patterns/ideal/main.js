/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export function tail(arr: [number, number, number, number]): [number, number, number] {
  return match (arr) {
    [_, ...const rest] => rest,
  };
}

export function splitConfig(
  config: {host: string, port: number, debug: boolean, verbose: boolean},
): {connection: {host: string, port: number}, flags: {debug: boolean, verbose: boolean}} {
  return match (config) {
    {host: const h, port: const p, ...const flags} => ({
      connection: {host: h, port: p},
      flags,
    }),
  };
}

