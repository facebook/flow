/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

type T = Bar;
declare export component Foo() renders T;

declare export component Foo(foo: renders Bar) renders T;

type T = renders Bar;

type T = component() renders Bar;
