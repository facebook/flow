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
/* Promises
   cf. https://github.com/borisyankov/DefinitelyTyped/blob/master/es6-promises/es6-promises.d.ts
*/
interface PromiseConstructor {
    readonly prototype: Promise<any>;
    new<R = unknown>(callback: (
      resolve: (result: Promise<R> | R) => void,
      reject: (error: any) => void
    ) => unknown): Promise<R>;

    /**
     * Creates a new resolved promise for the provided value.
     * @param object A promise.
     * @returns A promise whose internal state matches the provided promise.
     */
    resolve<T = unknown>(object: Promise<T> | T): Promise<T>;
    /**
     * Creates a new rejected promise for the provided reason.
     * @param error The reason the promise was rejected.
     * @returns A new rejected Promise.
     */
    reject<T = unknown>(error: any): Promise<T>;
    /**
     * Creates a Promise that is resolved with an array of results when all of the provided Promises
     * resolve, or rejected when any Promise is rejected.
     * @param promises An iterable of Promises.
     * @returns A new Promise.
     */
    all<T extends Iterable<unknown>>(promises: T): Promise<
      T extends ReadonlyArray<unknown> ? {[K in keyof T]: Awaited<T[K]>} :
      T extends Iterable<infer V> ? Array<Awaited<V>> : any
    >;
    /**
     * Creates a Promise that is resolved or rejected when any of the provided Promises are resolved
     * or rejected.
     * @param promises An iterable of Promises.
     * @returns A new Promise.
     */
    race<T, Elem extends Promise<T> | T>(promises: Iterable<Elem>): Promise<T>;
}

declare var Promise: PromiseConstructor;
