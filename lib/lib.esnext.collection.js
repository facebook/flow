/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Modifications Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the
 * License at http://www.apache.org/licenses/LICENSE-2.0
 * THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION ANY IMPLIED
 * WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR A PARTICULAR PURPOSE,
 * MERCHANTABILITY OR NON-INFRINGEMENT.
 * See the Apache Version 2.0 License for specific language governing permissions
 * and limitations under the License.
 *
 * @flow
 */
// @lint-ignore-every LICENSELINT
interface ReadonlySetLike<T> {
  keys(): Iterator<T>;
  has(value: T): boolean;
  readonly size: number;
}

interface $ReadOnlySet<out T> {
    /** Takes a `Set` and returns a new `Set` containing elements in this `Set` but not in the given `Set`. */
    difference(
      // $FlowFixMe[incompatible-variance]
      other: ReadonlySet<T>
      // $FlowFixMe[incompatible-variance]
    ): Set<T>;
    /** Takes a `Set` and returns a new `Set` containing elements in both this `Set` and the given `Set`. */
    intersection(
      // $FlowFixMe[incompatible-variance]
      other: ReadonlySet<T>
      // $FlowFixMe[incompatible-variance]
    ): Set<T>;
    /** Takes a `Set` and returns a `boolean` indicating if this `Set` has no elements in common with the given `Set`. */
    isDisjointFrom(
      // $FlowFixMe[incompatible-variance]
      other: ReadonlySet<T>): boolean;
    /** Takes a `Set` and returns a `boolean` indicating if all elements of this `Set` are in the given `Set`. */
    isSubsetOf(
      // $FlowFixMe[incompatible-variance]
      other: ReadonlySet<T>
    ): boolean;
    /** Takes a `Set` and returns a `boolean` indicating if all elements of the given `Set` are in this `Set`. */
    isSupersetOf(
      // $FlowFixMe[incompatible-variance]
      other: ReadonlySet<T>
    ): boolean;
    /** Returns a new `Set` containing elements which are in either this `Set` or the given `Set`, but not in both. */
    symmetricDifference(
      // $FlowFixMe[incompatible-variance]
      other: ReadonlySet<T>
      // $FlowFixMe[incompatible-variance]
    ): Set<T>;
}
