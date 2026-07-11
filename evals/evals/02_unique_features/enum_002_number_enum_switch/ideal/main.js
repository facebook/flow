/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export enum HttpStatus of number {
  Ok = 200,
  Created = 201,
  BadRequest = 400,
  Unauthorized = 401,
  NotFound = 404,
  InternalError = 500,
}

export function isSuccess(status: HttpStatus): boolean {
  switch (status) {
    case HttpStatus.Ok:
    case HttpStatus.Created:
      return true;
    case HttpStatus.BadRequest:
    case HttpStatus.Unauthorized:
    case HttpStatus.NotFound:
    case HttpStatus.InternalError:
      return false;
  }
}

export function toStatusLine(status: HttpStatus): string {
  const code: number = status as number;
  switch (status) {
    case HttpStatus.Ok:
      return `${code} OK`;
    case HttpStatus.Created:
      return `${code} Created`;
    case HttpStatus.BadRequest:
      return `${code} Bad Request`;
    case HttpStatus.Unauthorized:
      return `${code} Unauthorized`;
    case HttpStatus.NotFound:
      return `${code} Not Found`;
    case HttpStatus.InternalError:
      return `${code} Internal Server Error`;
  }
}

export function retryable(status: HttpStatus): boolean {
  switch (status) {
    case HttpStatus.InternalError:
      return true;
    case HttpStatus.Ok:
    case HttpStatus.Created:
    case HttpStatus.BadRequest:
    case HttpStatus.Unauthorized:
    case HttpStatus.NotFound:
      return false;
  }
}
