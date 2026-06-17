/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

interface Object {}
declare var Object: Object; // ERROR: libdef override

interface I<
  Class extends abstract new (...args: any) => any = abstract new (...args: any) => any,
> {}
