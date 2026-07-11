/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

class AppError {
  message: string;
  constructor(message: string) {
    this.message = message;
  }
}

class NetworkError extends AppError {
  statusCode: number;
  constructor(message: string, statusCode: number) {
    super(message);
    this.statusCode = statusCode;
  }
}

class TimeoutError extends NetworkError {
  elapsedMs: number;
  constructor(message: string, statusCode: number, elapsedMs: number) {
    super(message, statusCode);
    this.elapsedMs = elapsedMs;
  }
}

class ValidationError extends AppError {
  field: string;
  constructor(message: string, field: string) {
    super(message);
    this.field = field;
  }
}

type Failure = TimeoutError | NetworkError | ValidationError;

// TODO: Implement
