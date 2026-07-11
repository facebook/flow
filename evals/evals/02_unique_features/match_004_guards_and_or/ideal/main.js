/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export function classifyHttpStatus(
  status: number,
): 'info' | 'success' | 'redirect' | 'client_error' | 'server_error' | 'unknown' {
  return match (status) {
    200 | 201 | 204 => 'success',
    301 | 302 | 304 => 'redirect',
    400 | 401 | 403 | 404 => 'client_error',
    500 | 502 | 503 => 'server_error',
    const s if (s >= 100 && s < 200) => 'info',
    const s if (s >= 200 && s < 300) => 'success',
    const s if (s >= 300 && s < 400) => 'redirect',
    const s if (s >= 400 && s < 500) => 'client_error',
    const s if (s >= 500 && s < 600) => 'server_error',
    _ => 'unknown',
  };
}

