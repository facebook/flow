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
 * A typed array of 64-bit signed integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
interface BigInt64Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike>
  extends $TypedArrayInternal<bigint, TArrayBuffer, BigInt64Array<ArrayBuffer>> {}

/**
 * A typed array of 64-bit unsigned integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
interface BigUint64Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike>
  extends $TypedArrayInternal<bigint, TArrayBuffer, BigUint64Array<ArrayBuffer>> {}
interface BigInt64ArrayConstructor extends $TypedArrayConstructor<bigint, BigInt64Array<ArrayBufferLike>, BigInt64Array<ArrayBuffer>> {
    new(length?: number): BigInt64Array<ArrayBuffer>;
    new(array: $ArrayLike<bigint> | Iterable<bigint>): BigInt64Array<ArrayBuffer>;
    new<TArrayBuffer extends ArrayBufferLike = ArrayBuffer>(buffer: TArrayBuffer, byteOffset?: number, length?: number): BigInt64Array<TArrayBuffer>;
    new(buffer: ArrayBuffer, byteOffset?: number, length?: number): BigInt64Array<ArrayBuffer>;
    new(array: $ArrayLike<bigint> | ArrayBuffer): BigInt64Array<ArrayBuffer>;
}
interface BigUint64ArrayConstructor extends $TypedArrayConstructor<bigint, BigUint64Array<ArrayBufferLike>, BigUint64Array<ArrayBuffer>> {
    new(length?: number): BigUint64Array<ArrayBuffer>;
    new(array: $ArrayLike<bigint> | Iterable<bigint>): BigUint64Array<ArrayBuffer>;
    new<TArrayBuffer extends ArrayBufferLike = ArrayBuffer>(buffer: TArrayBuffer, byteOffset?: number, length?: number): BigUint64Array<TArrayBuffer>;
    new(buffer: ArrayBuffer, byteOffset?: number, length?: number): BigUint64Array<ArrayBuffer>;
    new(array: $ArrayLike<bigint> | ArrayBuffer): BigUint64Array<ArrayBuffer>;
}
declare var BigInt64Array: BigInt64ArrayConstructor;
declare var BigUint64Array: BigUint64ArrayConstructor;

declare class BigInt {
  static (value: boolean | string | number | bigint | interface {} | ReadonlyArray<unknown>): bigint;
  /**
   * Clamps a BigInt value to the given number of bits, and returns that value as a signed integer.
   * @param bits The amount of bits available for the returned BigInt. Should be an integer between 0 and 2^53 - 1, inclusive.
   * @param bigint The BigInt value to clamp to fit into the supplied bits.
   */
  static asIntN(bits: number, bigint: bigint): bigint;
  /**
   * Clamps a BigInt value to the given number of bits, and returns that value as an unsigned integer.
   * @param bits The amount of bits available for the returned BigInt. Should be an integer between 0 and 2^53 - 1, inclusive.
   * @param bigint The BigInt value to clamp to fit into the supplied bits.
   */
  static asUintN(bits: number, bigint: bigint): bigint;
  /**
   * Converts a number to a string by using the current or specified locale.
   * @param locales A locale string or array of locale strings that contain one or more language or locale tags. If you include more than one locale string, list them in descending order of priority so that the first entry is the preferred locale. If you omit this parameter, the default locale of the JavaScript runtime is used.
   * @param options An object that contains one or more properties that specify comparison options.
   */
  toLocaleString(locales?: string | Array<string>, options?: Intl$NumberFormatOptions): string;
  /**
   * Returns a string representation of a BigInt. The trailing "n" is not part of the string.
   * @param radix An integer in the range 2 through 36 specifying the base to use for representing the BigInt value. Defaults to 10.
   */
  toString(radix?: number): string;
  /** Returns the wrapped primitive value of a BigInt object. */
  valueOf(): bigint;
}
