/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

// Ref types remain in the config, as this is how TS ref helpers handle refs anyway.

declare export component Foo(ref: Ref<HTMLElement>);

declare export component Foo(foo: string, ref: Ref<HTMLElement>);
