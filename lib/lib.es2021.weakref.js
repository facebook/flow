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
/**
 * A WeakRef object lets you hold a weak reference to another object,
 * without preventing that object from getting garbage-collected.
 *
 * @see {@link https://caniuse.com/?search=weakref} See browser support
 */
declare class WeakRef<T extends WeaklyReferenceable> {
  constructor(targetObject: T): void;
  /**
  * @return the WeakRef object's target object,
  * or undefined if the target object has been reclaimed.
  */
  deref(): T | void;
}

/**
 * This class provides a way to request that a cleanup callback be called after
 * a registered target has been garbage-collected.
 */
declare class FinalizationRegistry<
  THeldValue,
  TTarget extends WeaklyReferenceable = WeaklyReferenceable,
  TUnregisterToken extends WeaklyReferenceable = WeaklyReferenceable,
> {
  /**
   * @param cleanupCallbackFn The function that will be called after a
   * registered target has been garbage-collected.
   */
  constructor(cleanupCallbackFn: (THeldValue) => void): void;

  /**
   * Start listening for the garbage collection of the given target.
   *
   * @param target The target to listen to.
   *
   * @param heldValue The value that will be passed to the cleanupCallbackFn
   * when the target is garbage collected.
   *
   * @param unregisterToken A optional token that, if provided, can be passed to
   * the unregister() method to stop listening to the given target.
   */
  register(
    target: TTarget,
    heldValue: THeldValue,
    unregisterToken?: TUnregisterToken,
  ): void;

  /**
   * Stop listening for the garbage collection of all targets that were
   * registered with the given unregisterToken.
   *
   * @return true if at least one target was unregistered and false oterhwise.
   */
  unregister(unregisterToken: TUnregisterToken): boolean;
}
