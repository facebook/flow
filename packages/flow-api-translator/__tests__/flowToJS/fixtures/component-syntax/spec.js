/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

component Foo1() {}
component Foo2(foo: string, bar as BAR: string, ...rest: {a: string}) {
  return <div />;
}
component Foo3(ref: Ref<string>) {}
component Foo4() renders Foo3 {}

declare component Foo5();
type Foo6 = component();
