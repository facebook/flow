/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

declare function foo(
  named1: string,
  anonymous1,
  named2: number,
  anonymous2,
): void;

declare function foo_rest(...rest: string[]): void;

declare function foo_this(this: string): void;

declare function foo_this_and_reast(
  this: string,
  named: number,
  anonymous,
  ...rest: boolean[]
): void;
