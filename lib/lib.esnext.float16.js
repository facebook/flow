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
interface Float16Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> {
    [index: number]: number;

    /**
     * The ArrayBuffer instance referenced by the array.
     */
    readonly buffer: TArrayBuffer;
    /**
     * The length in bytes of the array.
     */
    byteLength: number;
    /**
     * The offset in bytes of the array.
     */
    byteOffset: number;
    /**
     * The length of the array.
     */
    length: number;

    /**
     * Returns the this object after copying a section of the array identified by start and end
     * to the same array starting at position target
     * @param target If target is negative, it is treated as length+target where length is the
     * length of the array.
     * @param start If start is negative, it is treated as length+start. If end is negative, it
     * is treated as length+end.
     * @param end If not specified, length of the this object is used as its default value.
     */
    copyWithin(target: number, start: number, end?: number): this;
    /**
     * Determines whether all the members of an array satisfy the specified test.
     * @param callback A function that accepts up to three arguments. The every method calls
     * the predicate function for each element in the array until the predicate returns a value
     * which is coercible to the Boolean value false, or until the end of the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    every<This>(callback: (this : This, value: number, index: number, array: this) => unknown, thisArg: This): boolean;
    /**
     * Returns the this object after filling the section identified by start and end with value
     * @param value value to fill array section with
     * @param start index to start filling the array at. If start is negative, it is treated as
     * length+start where length is the length of the array.
     * @param end index to stop filling the array at. If end is negative, it is treated as
     * length+end.
     */
    fill(value: number, start?: number, end?: number): this;
    /**
     * Returns the elements of an array that meet the condition specified in a callback function.
     * @param callback A function that accepts up to three arguments. The filter method calls
     * the predicate function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    filter<This>(callback: (this : This, value: number, index: number, array: this) => unknown, thisArg: This): Float16Array<ArrayBuffer>;
    /**
     * Returns the value of the first element in the array where predicate is true, and undefined
     * otherwise.
     * @param callback find calls predicate once for each element of the array, in ascending
     * order, until it finds one where predicate returns true. If such an element is found, find
     * immediately returns that element value. Otherwise, find returns undefined.
     * @param thisArg If provided, it will be used as the this value for each invocation of
     * predicate. If it is not provided, undefined is used instead.
     */
    find<This> (callback: (this : This, value: number, index: number, array: this) => unknown, thisArg: This): number | void;
    /**
     * Returns the index of the first element in the array where predicate is true, and -1
     * otherwise.
     * @param callback find calls predicate once for each element of the array, in ascending
     * order, until it finds one where predicate returns true. If such an element is found,
     * findIndex immediately returns that element index. Otherwise, findIndex returns -1.
     * @param thisArg If provided, it will be used as the this value for each invocation of
     * predicate. If it is not provided, undefined is used instead.
     */
    findIndex<This>(callback: (this : This, value: number, index: number, array: this) => unknown, thisArg: This): number;
    /**
     * Performs the specified action for each element in an array.
     * @param callback  A function that accepts up to three arguments. forEach calls the
     * callbackfn function one time for each element in the array.
     * @param thisArg  An object to which the this keyword can refer in the callbackfn function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    forEach<This>(callback: (this : This, value: number, index: number, array: this) => unknown, thisArg: This): void;
    /**
     * Returns the index of the first occurrence of a value in an array.
     * @param searchElement The value to locate in the array.
     * @param fromIndex The array index at which to begin the search. If fromIndex is omitted, the
     *  search starts at index 0.
     */
    indexOf(searchElement: number, fromIndex?: number): number; // -1 if not present
    /**
     * Adds all the elements of an array separated by the specified separator string.
     * @param separator A string used to separate one element of an array from the next in the
     * resulting String. If omitted, the array elements are separated with a comma.
     */
    join(separator?: string): string;
    /**
     * Returns the index of the last occurrence of a value in an array.
     * @param searchElement The value to locate in the array.
     * @param fromIndex The array index at which to begin the search. If fromIndex is omitted, the
     * search starts at index 0.
     */
    lastIndexOf(searchElement: number, fromIndex?: number): number; // -1 if not present
    /**
     * Calls a defined callback function on each element of an array, and returns an array that
     * contains the results.
     * @param callback A function that accepts up to three arguments. The map method calls the
     * callbackfn function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the callbackfn function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    map<This>(callback: (this : This, currentValue: number, index: number, array: this) => number, thisArg : This): Float16Array<ArrayBuffer>;
    /**
     * Calls the specified callback function for all the elements in an array. The return value of
     * the callback function is the accumulated result, and is provided as an argument in the next
     * call to the callback function.
     * @param callback A function that accepts up to four arguments. The reduce method calls the
     * callbackfn function one time for each element in the array.
     * @param initialValue If initialValue is specified, it is used as the initial value to start
     * the accumulation. The first call to the callbackfn function provides this value as an argument
     * instead of an array value.
     */
    reduce(
      callback: (previousValue: number, currentValue: number, index: number, array: this) => number,
      initialValue: void
    ): number;
    /**
     * Calls the specified callback function for all the elements in an array. The return value of
     * the callback function is the accumulated result, and is provided as an argument in the next
     * call to the callback function.
     * @param callback A function that accepts up to four arguments. The reduce method calls the
     * callbackfn function one time for each element in the array.
     * @param initialValue If initialValue is specified, it is used as the initial value to start
     * the accumulation. The first call to the callbackfn function provides this value as an argument
     * instead of an array value.
     */
    reduce<U>(
      callback: (previousValue: U, currentValue: number, index: number, array: this) => U,
      initialValue: U
    ): U;
    /**
     * Calls the specified callback function for all the elements in an array, in descending order.
     * The return value of the callback function is the accumulated result, and is provided as an
     * argument in the next call to the callback function.
     * @param callback A function that accepts up to four arguments. The reduceRight method calls
     * the callbackfn function one time for each element in the array.
     * @param initialValue If initialValue is specified, it is used as the initial value to start
     * the accumulation. The first call to the callbackfn function provides this value as an
     * argument instead of an array value.
     */
    reduceRight(
      callback: (previousValue: number, currentValue: number, index: number, array: this) => number,
      initialValue: void
    ): number;
    /**
     * Calls the specified callback function for all the elements in an array, in descending order.
     * The return value of the callback function is the accumulated result, and is provided as an
     * argument in the next call to the callback function.
     * @param callback A function that accepts up to four arguments. The reduceRight method calls
     * the callbackfn function one time for each element in the array.
     * @param initialValue If initialValue is specified, it is used as the initial value to start
     * the accumulation. The first call to the callbackfn function provides this value as an
     * argument instead of an array value.
     */
    reduceRight<U>(
      callback: (previousValue: U, currentValue: number, index: number, array: this) => U,
      initialValue: U
    ): U;
    /**
     * Reverses the elements in an Array.
     */
    reverse(): this;
    /**
     * Sets a value or an array of values.
     * @param array A typed or untyped array of values to set.
     * @param offset The index in the current array at which the values are to be written.
     */
    set(array: ArrayLike<number>, offset?: number): void;
    /**
     * Returns a section of an array.
     * @param start The beginning of the specified portion of the array.
     * @param end The end of the specified portion of the array. This is exclusive of the element at the index 'end'.
     */
    slice(begin?: number, end?: number): Float16Array<ArrayBuffer>;
    /**
     * Determines whether the specified callback function returns true for any element of an array.
     * @param callback A function that accepts up to three arguments. The some method calls
     * the predicate function for each element in the array until the predicate returns a value
     * which is coercible to the Boolean value true, or until the end of the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    some<This>(callback: (this : This, value: number, index: number, array: this) => unknown, thisArg: This): boolean;
    /**
     * Sorts an array.
     * @param compareFn Function used to determine the order of the elements. It is expected to return
     * a negative value if first argument is less than second argument, zero if they're equal and a positive
     * value otherwise. If omitted, the elements are sorted in ascending, ASCII character order.
     * ```ts
     * [11,2,22,1].sort((a, b) => a - b)
     * ```
     */
    sort(compare?: (a: number, b: number) => number): this;
    /**
     * Gets a new Float16Array view of the ArrayBuffer store for this array, referencing the
     * elements at begin, inclusive, up to end, exclusive.
     * @param begin The index of the beginning of the array.
     * @param end The index of the end of the array.
     */
    subarray(begin?: number, end?: number): this;
}
interface Float16ArrayConstructor extends $TypedArrayConstructor<number, Float16Array<ArrayBufferLike>, Float16Array<ArrayBuffer>> {
    new(length?: number): Float16Array<ArrayBuffer>;
    new(array: ArrayLike<number> | Iterable<number>): Float16Array<ArrayBuffer>;
    new<TArrayBuffer extends ArrayBufferLike = ArrayBuffer>(buffer: TArrayBuffer, byteOffset?: number, length?: number): Float16Array<TArrayBuffer>;
    new(buffer: ArrayBuffer, byteOffset?: number, length?: number): Float16Array<ArrayBuffer>;
    new(array: ArrayLike<number> | ArrayBuffer): Float16Array<ArrayBuffer>;
}
declare var Float16Array: Float16ArrayConstructor;

interface Math {
    /**
     * Returns the nearest 16-bit half precision float representation of a number.
     * @param x A numeric expression.
     */
    f16round(x: number): number,
}

interface DataView<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> {
    /**
     * Gets the Float16 value at the specified byte offset from the start of the view. There is
     * no alignment constraint; multi-byte values may be fetched from any offset.
     * @param byteOffset The place in the buffer at which the value should be retrieved.
     */
    getFloat16(byteOffset: number, littleEndian?: boolean): number;
    /**
     * Stores an Float16 value at the specified byte offset from the start of the view.
     * @param byteOffset The place in the buffer at which the value should be set.
     * @param value The value to set.
     * @param littleEndian If false or undefined, a big-endian value should be written,
     * otherwise a little-endian value should be written.
     */
    setFloat16(byteOffset: number, value: number, littleEndian?: boolean): void;
}
