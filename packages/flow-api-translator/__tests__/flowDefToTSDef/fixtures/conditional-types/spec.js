/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

type TypeOf<T> =
  T extends null ? 'null' :
  T extends void ? 'undefined' :
  T extends string ? 'string' :
  T extends number ? 'number' :
  T extends boolean ? 'boolean' :
  T extends (...$ReadOnlyArray<any>)=>mixed ? 'function' : 'object';
