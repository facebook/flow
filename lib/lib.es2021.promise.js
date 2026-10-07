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
interface AggregateError extends Error {
  errors: Iterable<unknown>;
}

interface AggregateErrorConstructor {
  readonly prototype: AggregateError;
  (errors: Iterable<unknown>, message?: string): Error;
  new(errors: Iterable<unknown>, message?: unknown): AggregateError;
}

declare var AggregateError: AggregateErrorConstructor;

interface PromiseConstructor {
    /**
     * Creates a Promise that fulfills as soon as any of the promises in the iterable fulfills,
     * with the value of the fulfilled promise. If no promises in the iterable fulfill then the
     * returned promise is rejected with an AggregateError.
     * @param promises An iterable of Promises.
     * @returns A new Promise.
     */
    any<T, Elem extends Promise<T> | T>(promises: Iterable<Elem>): Promise<T>;
}
