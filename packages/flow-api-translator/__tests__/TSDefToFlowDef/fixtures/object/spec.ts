/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

type Foo1 = {};
type Foo2 = {
  constructor(): void;
  property: string;
  method(): string;
  (this: void, arg1: string, anonymous, ...rest: string[]): string;
  get foo(): string;
  set foo(v: string): void;
};
