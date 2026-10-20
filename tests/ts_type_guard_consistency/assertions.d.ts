/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

export interface Parent {
  check(value: unknown): asserts value is string;
}

export interface Child extends Parent {
  check(value: unknown): void;
}

export interface Bad extends Parent { // ERROR
  check: string;
}
