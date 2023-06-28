/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

type _Foo = string extends number ? string : number;

type Obj = {a: string};
type _Bar = {+[K in keyof Obj]: Obj[K]};

function foo(x: mixed): x is string {
  return typeof x === 'string';
}
