/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export enum ApiCode {
  Success,
  RateLimit,
  Maintenance,
  ...
}

export function handleResponse(code: ApiCode): string {
  switch (code) {
    case ApiCode.Success:
      return 'proceed';
    case ApiCode.RateLimit:
      return 'backoff';
    case ApiCode.Maintenance:
      return 'retry later';
    default:
      return 'unknown response';
  }
}

export function isSafeToRetry(code: ApiCode): boolean {
  switch (code) {
    case ApiCode.RateLimit:
    case ApiCode.Maintenance:
      return true;
    case ApiCode.Success:
    default:
      return false;
  }
}
