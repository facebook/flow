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
interface Math {
    /**
     * Returns the inverse hyperbolic cosine of a number.
     * @param x A numeric expression that contains an angle measured in radians.
     */
    acosh(x: number): number,
    /**
     * Returns the inverse hyperbolic sine of a number.
     * @param x A numeric expression that contains an angle measured in radians.
     */
    asinh(x: number): number,
    /**
     * Returns the inverse hyperbolic tangent of a number.
     * @param x A numeric expression that contains an angle measured in radians.
     */
    atanh(x: number): number,
    /**
     * Returns an implementation-dependent approximation to the cube root of number.
     * @param x A numeric expression.
     */
    cbrt(x: number): number,
    /**
     * Returns the number of leading zero bits in the 32-bit binary representation of a number.
     * @param x A numeric expression.
     */
    clz32(x: number): number,
    /**
     * Returns the hyperbolic cosine of a number.
     * @param x A numeric expression that contains an angle measured in radians.
     */
    cosh(x: number): number,
    /**
     * Returns the result of (e^x - 1), which is an implementation-dependent approximation to
     * subtracting 1 from the exponential function of x (e raised to the power of x, where e
     * is the base of the natural logarithms).
     * @param x A numeric expression.
     */
    expm1(x: number): number,
    /**
     * Returns the nearest single precision float representation of a number.
     * @param x A numeric expression.
     */
    fround(x: number): number,
    /**
     * Returns the square root of the sum of squares of its arguments.
     * @param values Values to compute the square root for.
     *     If no arguments are passed, the result is +0.
     *     If there is only one argument, the result is the absolute value.
     *     If any argument is +Infinity or -Infinity, the result is +Infinity.
     *     If any argument is NaN, the result is NaN.
     *     If all arguments are either +0 or -0, the result is +0.
     */
    hypot(...values: Array<number>): number,
    /**
     * Returns the result of 32-bit multiplication of two numbers.
     * @param x First number
     * @param y Second number
     */
    imul(x: number, y: number): number,
    /**
     * Returns the base 10 logarithm of a number.
     * @param x A numeric expression.
     */
    log10(x: number): number,
    /**
     * Returns the natural logarithm of 1 + x.
     * @param x A numeric expression.
     */
    log1p(x: number): number,
    /**
     * Returns the base 2 logarithm of a number.
     * @param x A numeric expression.
     */
    log2(x: number): number,
    /**
     * Returns the sign of the x, indicating whether x is positive, negative or zero.
     * @param x The numeric expression to test
     */
    sign(x: number): number,
    /**
     * Returns the hyperbolic sine of a number.
     * @param x A numeric expression that contains an angle measured in radians.
     */
    sinh(x: number): number,
    /**
     * Returns the hyperbolic tangent of a number.
     * @param x A numeric expression that contains an angle measured in radians.
     */
    tanh(x: number): number,
    /**
     * Returns the integral part of the a numeric expression, x, removing any fractional digits.
     * If x is already an integer, the result is x.
     * @param x A numeric expression.
     */
    trunc(x: number): number,
}

interface NumberConstructor {
    /**
     * The value of Number.EPSILON is the difference between 1 and the smallest value greater than 1
     * that is representable as a Number value, which is approximately:
     * 2.2204460492503130808472633361816 x 10^-16.
     */
    EPSILON: number;
    /**
     * The value of the largest integer n such that n and n + 1 are both exactly representable as
     * a Number value.
     * The value of Number.MAX_SAFE_INTEGER is 9007199254740991 2^53 - 1.
     */
    MAX_SAFE_INTEGER: number;
    /**
     * The value of the smallest integer n such that n and n - 1 are both exactly representable as
     * a Number value.
     * The value of Number.MIN_SAFE_INTEGER is -9007199254740991 (-(2^53 - 1)).
     */
    MIN_SAFE_INTEGER: number;
    /**
     * Returns true if passed value is finite.
     * Unlike the global isFinite, Number.isFinite doesn't forcibly convert the parameter to a
     * number. Only finite values of the type number, result in true.
     * @param value A numeric value.
     */
    isFinite(value: unknown): implies value is number;
    /**
     * Returns true if the value passed is an integer, false otherwise.
     * @param value A numeric value.
     */
    isInteger(value: unknown): implies value is number;
    /**
     * Returns a Boolean value that indicates whether a value is the reserved value NaN (not a
     * number). Unlike the global isNaN(), Number.isNaN() doesn't forcefully convert the parameter
     * to a number. Only values of the type number, that are also NaN, result in true.
     * @param value A numeric value.
     */
    isNaN(value: unknown): implies value is number;
    /**
     * Returns true if the value passed is a safe integer.
     * @param value A numeric value.
     */
    isSafeInteger(value: unknown): implies value is number;
    /**
     * Converts a string to a floating-point number.
     * @param value A string that contains a floating-point number.
     */
    parseFloat(value: string): number;
    /**
     * Converts A string to an integer.
     * @param value A string to convert into a number.
     * @param radix A value between 2 and 36 that specifies the base of the number in numString.
     * If this argument is not supplied, strings with a prefix of '0x' are considered hexadecimal.
     * All other strings are considered decimal.
     */
    parseInt(value: string, radix?: number): number;
}

interface RegExp {
    /**
     * Returns a string indicating the flags of the regular expression in question. This field is read-only.
     * The characters in this string are sequenced and concatenated in the following order:
     *
     *    - "g" for global
     *    - "i" for ignoreCase
     *    - "m" for multiline
     *    - "u" for unicode
     *    - "y" for sticky
     *
     * If no flags are set, the value is the empty string.
     */
    flags: string;
    /**
     * Returns a Boolean value indicating the state of the sticky flag (y) used with a regular
     * expression. Default is false. Read-only.
     */
    sticky: boolean;
    /**
     * Returns a Boolean value indicating the state of the Unicode flag (u) used with a regular
     * expression. Default is false. Read-only.
     */
    unicode: boolean;
}

interface String {
    /**
     * Returns an `<a>` HTML anchor element and sets the name attribute to the text value
     * @param name
     */
    anchor(name: string): string;
    /**
     * Returns a nonnegative integer Number less than 1114112 (0x110000) that is the code point
     * value of the UTF-16 encoded code point starting at the string element at position index in
     * the String resulting from converting this object to a String.
     * If there is no element at that position, the result is undefined.
     * If a valid UTF-16 surrogate pair does not begin at index, the result is the code unit at index.
     */
    codePointAt(index: number): number | void;
    /**
     * Returns true if the sequence of elements of searchString converted to a String is the
     * same as the corresponding elements of this object (converted to a String) starting at
     * position - length(this). Otherwise returns false.
     */
    endsWith(searchString: string, position?: number): boolean;
    /**
     * Returns true if searchString appears as a substring of the result of converting this
     * object to a String, at one or more positions that are
     * greater than or equal to position; otherwise, returns false.
     * @param searchString search string
     * @param position If position is undefined, 0 is assumed, so as to search all of the String.
     */
    includes(searchString: string, position?: number): boolean;
    /** Returns an `<a>` HTML element and sets the href attribute value */
    link(href: string): string;
    /**
     * Returns the String value result of normalizing the string into the normalization form
     * named by form as specified in Unicode Standard Annex #15, Unicode Normalization Forms.
     * @param format Applicable values: "NFC", "NFD", "NFKC", or "NFKD", If not specified default
     * is "NFC"
     */
    normalize(format?: string): string;
    /**
     * Returns a String value that is made from count copies appended together. If count is 0,
     * the empty string is returned.
     * @param count number of copies to append
     */
    repeat(count: number): string;
    /**
     * Returns true if the sequence of elements of searchString converted to a String is the
     * same as the corresponding elements of this object (converted to a String) starting at
     * position. Otherwise returns false.
     */
    startsWith(searchString: string, position?: number): boolean;
}

interface StringConstructor {
    /**
     * Return the String value whose elements are, in order, the elements in the List elements.
     * If length is 0, the empty string is returned.
     */
    fromCodePoint(...codes: Array<number>): string;

    /**
     * String.raw is intended for use as a tag function of a Tagged Template String. When called
     * as such the first argument will be a well formed template call site object and the rest
     * parameter will contain the substitution values.
     * @param callSite A well-formed template string call site representation.
     * @param substitutions A set of substitution values.
     */
    raw: (callSite: interface {readonly raw: ReadonlyArray<string>}, ...substitutions: ReadonlyArray<unknown>) => string;
}
