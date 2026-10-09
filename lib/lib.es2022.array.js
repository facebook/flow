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
interface ReadonlyArray<out T> {
    /**
     * Returns the item located at the specified index.
     * @param index The zero-based index of the desired item. A negative index will count back from the last item.
     */
    at(index: number): T | void;
}

interface Int8Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> {
    /**
     * Returns the item located at the specified index.
     * @param index The zero-based index of the desired code unit. A negative index will count back from the last item.
     */
    at(index: number): number | undefined;
}

interface Uint8Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> {
    /**
     * Returns the item located at the specified index.
     * @param index The zero-based index of the desired code unit. A negative index will count back from the last item.
     */
    at(index: number): number | undefined;
}

interface Uint8ClampedArray<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> {
    /**
     * Returns the item located at the specified index.
     * @param index The zero-based index of the desired code unit. A negative index will count back from the last item.
     */
    at(index: number): number | undefined;
}

interface Int16Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> {
    /**
     * Returns the item located at the specified index.
     * @param index The zero-based index of the desired code unit. A negative index will count back from the last item.
     */
    at(index: number): number | undefined;
}

interface Uint16Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> {
    /**
     * Returns the item located at the specified index.
     * @param index The zero-based index of the desired code unit. A negative index will count back from the last item.
     */
    at(index: number): number | undefined;
}

interface Int32Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> {
    /**
     * Returns the item located at the specified index.
     * @param index The zero-based index of the desired code unit. A negative index will count back from the last item.
     */
    at(index: number): number | undefined;
}

interface Uint32Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> {
    /**
     * Returns the item located at the specified index.
     * @param index The zero-based index of the desired code unit. A negative index will count back from the last item.
     */
    at(index: number): number | undefined;
}

interface Float32Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> {
    /**
     * Returns the item located at the specified index.
     * @param index The zero-based index of the desired code unit. A negative index will count back from the last item.
     */
    at(index: number): number | undefined;
}

interface Float64Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> {
    /**
     * Returns the item located at the specified index.
     * @param index The zero-based index of the desired code unit. A negative index will count back from the last item.
     */
    at(index: number): number | undefined;
}

interface BigInt64Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> {
    /**
     * Returns the item located at the specified index.
     * @param index The zero-based index of the desired code unit. A negative index will count back from the last item.
     */
    at(index: number): bigint | undefined;
}

interface BigUint64Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> {
    /**
     * Returns the item located at the specified index.
     * @param index The zero-based index of the desired code unit. A negative index will count back from the last item.
     */
    at(index: number): bigint | undefined;
}
