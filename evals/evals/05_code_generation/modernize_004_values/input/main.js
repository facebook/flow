/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type StatusCodes = {
  ok: 200,
  notFound: 404,
  serverError: 500,
};

type StatusCode = $Values<StatusCodes>;

export function isError(code: StatusCode): boolean {
  return code >= 400;
}
