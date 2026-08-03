/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

type A = typeof foo.default;
type B = typeof foo.default.b;
type C = typeof undefined;
type D = typeof _;
type E = typeof type;
