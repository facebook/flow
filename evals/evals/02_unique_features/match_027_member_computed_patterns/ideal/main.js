/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

const HttpStatus = {
  tooManyRequests: 429,
  serverError: 500,
  badGateway: 502,
  gatewayTimeout: 504,
} as const;

type RetryableStatus = 429 | 500 | 502 | 504;

export function retryDelayMs(status: RetryableStatus, attempt: number): number {
  return match (status) {
    HttpStatus.tooManyRequests => 1000 * attempt,
    HttpStatus.serverError => 250 * 2 ** attempt,
    HttpStatus['badGateway'] => 250 * 2 ** attempt,
    HttpStatus['gatewayTimeout'] => 500 * 2 ** attempt,
  };
}
