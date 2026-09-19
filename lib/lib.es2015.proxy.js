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
/* Proxy */

type Proxy$traps<T> = {
    getPrototypeOf?: (target: T) => {[any] : any, ...} | null,
    setPrototypeOf?: (target: T, prototype: {[any] : any, ...} | null) => boolean,
    isExtensible?: (target: T) => boolean,
    preventExtensions?: (target: T) => boolean,
    getOwnPropertyDescriptor?: (target: T, property: string) => void | PropertyDescriptor<T>,
    defineProperty?: (target: T, property: string, descriptor: PropertyDescriptor<T>) => boolean,
    has?: (target: T, key: string) => boolean,
    get?: (target: T, property: string, receiver: Proxy<T>) => any,
    set?: (target: T, property: string, value: any, receiver: Proxy<T>) => boolean,
    deleteProperty?: (target: T, property: string) => boolean,
    ownKeys?: (target: T) => Array<string>,
    apply?: (target: T, context: any, args: Array<any>) => any,
    construct?: (target: T, args: Array<any>, newTarget: (...any) => any) => {[any] : any, ...},
    ...
};

type Proxy$revocable<T> = T & { revoke(): void, ... };

declare class Proxy<T> {
  constructor(target: T, handler: Proxy$traps<T>): T;

  static revocable(target: T, handler: Proxy$traps<T>): Proxy$revocable<T>;
}
type ProxyHandler<T> = Proxy$traps<T>;
