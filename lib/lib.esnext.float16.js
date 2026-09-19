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
 * A typed array of 16-bit float values. The contents are initialized to 0. If the requested number
 * of bytes could not be allocated an exception is raised.
 */
interface Float16Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike>
  extends $TypedArrayInternal<number, TArrayBuffer, Float16Array<ArrayBuffer>> {}
interface Float16ArrayConstructor extends $TypedArrayConstructor<number, Float16Array<ArrayBufferLike>, Float16Array<ArrayBuffer>> {
    new(length?: number): Float16Array<ArrayBuffer>;
    new(array: $ArrayLike<number> | Iterable<number>): Float16Array<ArrayBuffer>;
    new<TArrayBuffer extends ArrayBufferLike = ArrayBuffer>(buffer: TArrayBuffer, byteOffset?: number, length?: number): Float16Array<TArrayBuffer>;
    new(buffer: ArrayBuffer, byteOffset?: number, length?: number): Float16Array<ArrayBuffer>;
    new(array: $ArrayLike<number> | ArrayBuffer): Float16Array<ArrayBuffer>;
}
declare var Float16Array: Float16ArrayConstructor;
