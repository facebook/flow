/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

type T = component();

type T = component(foo: string, 'bar'?: string);

type ExtraPropsFoo = {foo: string};
type T = component(...rest: ExtraPropsFoo);

type ExtraPropsBar = {'bar': string};
type T = component(ref?: React.RefSetter<Foo>, foo: string, ...rest: ExtraPropsBar);
