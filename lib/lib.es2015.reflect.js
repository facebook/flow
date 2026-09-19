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
/* Reflect API */

declare var Reflect: {
    apply(target: (...any) => any, thisArg?: any, argumentsList?: Array<any>): any,
    construct(target: (...any) => any, argumentsList?: Array<any>, newTarget?: any): any,
    defineProperty(o: any, p: any, attributes: any): boolean,
    deleteProperty(o: any, p: any): boolean,
    get(o: any, p: any, receiver?: any): any,
    getOwnPropertyDescriptor(o: any, p: any): any,
    getPrototypeOf: typeof Object.getPrototypeOf,
    setPrototypeOf: (target: any, prototype: {[any] : any, ...} | null) => boolean,
    has(o: any, p: any): boolean,
    isExtensible(o: any): boolean,
    ownKeys(o: any): Array<any>,
    preventExtensions(o: any): boolean,
    set(o: any, p: any, value: any, receiver?: any): boolean,
    ...
}
