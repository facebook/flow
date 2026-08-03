/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

declare export component Foo();

declare export component Foo(foo: string, 'bar' as bar?: string);

type ExtraPropsFoo = {foo: string};
declare export component Foo(...rest: ExtraPropsFoo);

type ExtraPropsBar = {'bar': string};
declare export component Foo(foo: string, ...rest: ExtraPropsBar);
