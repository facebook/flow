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
declare var NaN: number;
declare var Infinity: number;

/**
 * Converts a string to an integer.
 * @param string A string to convert into a number.
 * @param radix A value between 2 and 36 that specifies the base of the number in numString.
 * If this argument is not supplied, strings with a prefix of '0x' are considered hexadecimal.
 * All other strings are considered decimal.
 */
declare function parseInt(string: unknown, radix?: number): number;
/**
 * Converts a string to a floating-point number.
 * @param string A string that contains a floating-point number.
 */
declare function parseFloat(string: unknown): number;

/**
 * Returns a boolean value that indicates whether a value is the reserved value NaN (not a number).
 * @param number A numeric value.
 */
declare function isNaN(number: unknown): boolean;
/**
 * Determines whether a supplied number is finite.
 * @param number Any numeric value.
 */
declare function isFinite(number: unknown): boolean;
/**
 * Gets the unencoded version of an encoded Uniform Resource Identifier (URI).
 * @param encodedURI A value representing an encoded URI.
 */
declare function decodeURI(encodedURI: string): string;
/**
 * Gets the unencoded version of an encoded component of a Uniform Resource Identifier (URI).
 * @param encodedURIComponent A value representing an encoded URI component.
 */
declare function decodeURIComponent(encodedURIComponent: string): string;
/**
 * Encodes a text string as a valid Uniform Resource Identifier (URI)
 * @param uri A value representing an encoded URI.
 */
declare function encodeURI(uri: string): string;
/**
 * Encodes a text string as a valid component of a Uniform Resource Identifier (URI).
 * @param uriComponent A value representing an encoded URI component.
 */
declare function encodeURIComponent(uriComponent: string): string;

type PropertyDescriptor<T> = {
    readonly enumerable?: boolean,
    readonly configurable?: boolean,
    readonly writable?: boolean,
    readonly value?: T,
    readonly get?: () => T,
    readonly set?: (value: T) => void,
    ...
};

type PropertyDescriptorMap = { [s: string]: PropertyDescriptor<any>, ... }

declare class Object {
    static (o: ?void): { [key: any]: any, ... };
    static (o: boolean): Boolean;
    static (o: number): Number;
    static (o: string): String;
    static <T>(o: T): T;
    /**
     * Copy the values of all of the enumerable own properties from one or more source objects to a
     * target object. Returns the target object.
     * @param target The target object to copy to.
     * @param sources The source object from which to copy properties.
     */
    static assign: Object$Assign;
    /**
     * Creates an object that has the specified prototype, and that optionally contains specified properties.
     * @param o Object to use as a prototype. May be null
     * @param properties JavaScript object that contains one or more property descriptors.
     */
    static create(o: any, properties?: PropertyDescriptorMap): any; // compiler magic
    /**
     * Adds one or more properties to an object, and/or modifies attributes of existing properties.
     * @param o Object on which to add or modify the properties. This can be a native JavaScript object or a DOM object.
     * @param properties JavaScript object that contains one or more descriptor objects. Each descriptor object describes a data property or an accessor property.
     */
    static defineProperties(o: any, properties: PropertyDescriptorMap): any;
    /**
     * Adds a property to an object, or modifies attributes of an existing property.
     * @param o Object on which to add or modify the property. This can be a native JavaScript object (that is, a user-defined object or a built in object) or a DOM object.
     * @param p The property name.
     * @param attributes Descriptor for the property. It can be for a data property or an accessor property.
     */
    static defineProperty<T>(o: any, p: any, attributes: PropertyDescriptor<T>): any;
    /**
     * Returns an array of key/values of the enumerable properties of an object
     * @param object Object that contains the properties and methods. This can be an object that you created or an existing Document Object Model (DOM) object.
     */
    static entries(object: interface {}): Array<[string, unknown]>;
    /**
     * Prevents the modification of existing property attributes and values, and prevents the addition of new properties.
     * @param o Object on which to lock the attributes.
     */
    static freeze<T>(o: T): T;
    /**
     * Returns an object created by key-value entries for properties and methods
     * @param entries An iterable object that contains key-value entries for properties and methods.
     */
    static fromEntries<K, V>(entries: Iterable<[K, V] | {
        '0': K,
        '1': V,
        ...
    }>): { [K]: V, ... };

    /**
     * Gets the own property descriptor of the specified object.
     * An own property descriptor is one that is defined directly on the object and is not inherited from the object's prototype.
     * @param o Object that contains the property.
     * @param p Name of the property.
     */
    static getOwnPropertyDescriptor<T = unknown>(o: $NotNullOrVoid, p: any): PropertyDescriptor<T> | void;
    /**
     * Gets the own property descriptors of the specified object.
     * An own property descriptor is one that is defined directly on the object and is not inherited from the object's prototype.
     * @param o Object that contains the properties.
     */
    static getOwnPropertyDescriptors(o: {...}): PropertyDescriptorMap;
    // This is documentation only. Object.getOwnPropertyNames is implemented in OCaml code
    // https://github.com/facebook/flow/blob/8ac01bc604a6827e6ee9a71b197bb974f8080049/src/typing/statement.ml#L6308
    /**
     * Returns the names of the own properties of an object. The own properties of an object are those that are defined directly
     * on that object, and are not inherited from the object's prototype. The properties of an object include both fields (objects) and functions.
     * @param o Object that contains the own properties.
     */
    static getOwnPropertyNames(o: $NotNullOrVoid): Array<string>;
    /**
     * Returns an array of all symbol properties found directly on object o.
     * @param o Object to retrieve the symbols from.
     */
    static getOwnPropertySymbols(o: $NotNullOrVoid): Array<symbol>;
    /**
     * Returns the prototype of an object.
     * @param o The object that references the prototype.
     */
    static getPrototypeOf(o: $NotNullOrVoid): any;
    /**
     * Returns true if the specified object has the indicated property as its own property.
     * If the property is inherited, or does not exist, the method returns false.
     * @param obj The JavaScript object instance to test.
     * @param prop The String name or Symbol of the property to test.
     */
    static hasOwn(obj: $NotNullOrVoid, prop: unknown): boolean;
    /**
     * Returns true if the values are the same value, false otherwise.
     * @param a The first value.
     * @param b The second value.
     */
    static is<T>(a: T, b: T): boolean;
    /**
     * Returns a value that indicates whether new properties can be added to an object.
     * @param o Object to test.
     */
    static isExtensible(o: $NotNullOrVoid): boolean;
    /**
     * Returns true if existing property attributes and values cannot be modified in an object, and new properties cannot be added to the object.
     * @param o Object to test.
     */
    static isFrozen(o: $NotNullOrVoid): boolean;
    static isSealed(o: $NotNullOrVoid): boolean;
    // This is documentation only. Object.keys is implemented in OCaml code.
    // https://github.com/facebook/flow/blob/8ac01bc604a6827e6ee9a71b197bb974f8080049/src/typing/statement.ml#L6308
    /**
     * Returns the names of the enumerable string properties and methods of an object.
     * @param o Object that contains the properties and methods. This can be an object that you created or an existing Document Object Model (DOM) object.
     */
    static keys(o: interface {}): Array<string>;
    /**
     * Prevents the addition of new properties to an object.
     * @param o Object to make non-extensible.
     */
    static preventExtensions<T>(o: T): T;
    /**
     * Prevents the modification of attributes of existing properties, and prevents the addition of new properties.
     * @param o Object on which to lock the attributes.
     */
    static seal<T>(o: T): T;
    /**
     * Sets the prototype of a specified object o to object proto or null. Returns the object o.
     * @param o The object to change its prototype.
     * @param proto The value of the new prototype or null.
     */
    static setPrototypeOf<T>(o: T, proto: ?{...}): T;
    /**
     * Returns an array of values of the enumerable properties of an object
     * @param object Object that contains the properties and methods. This can be an object that you created or an existing Document Object Model (DOM) object.
     */
    static values(object: interface {}): Array<unknown>;
    /**
     * Groups members of an iterable according to the return value of the passed callback.
     * @param items An iterable.
     * @param keySelector A callback which will be invoked for each item in items.
     */
    static groupBy<T, K extends string | number | bigint | boolean | symbol>(items: Iterable<T>, keySelector: (item: T, index: number) => K): {[K]: Array<T> | void};
    /**
     * Determines whether an object has a property with the specified name.
     * @param prop A property name.
     */
    hasOwnProperty(prop: unknown): boolean;
    /**
     * Determines whether an object exists in another object's prototype chain.
     * @param o Another object whose prototype chain is to be checked.
     */
    isPrototypeOf(o: unknown): boolean;
    /**
     * Determines whether a specified property is enumerable.
     * @param prop A property name.
     */
    propertyIsEnumerable(prop: unknown): boolean;
    /** Returns a date converted to a string using the current locale. */
    toLocaleString(): string;
    /** Returns a string representation of an object. */
    toString(): string;
    /** Returns the primitive value of the specified object. */
    valueOf(): unknown;
}

// TODO: instance, static
declare class Function {
    proto apply: (<T, R, A extends ArrayLike<unknown> = []>(this: (this: T, ...args: A) => R, thisArg: T, args?: A) => R);
    proto bind: Function$Prototype$Bind; // (thisArg: any, ...argArray: Array<any>) => any;
    proto call: <T, R, A extends ArrayLike<unknown> = []>(this: (this: T, ...args: A) => R, thisArg: T, ...args: A) => R;
    /** Returns a string representation of a function. */
    toString(): string;
    arguments: any;
    caller: any | null;
    readonly length: number;
}

declare class Boolean {
    constructor(value?: unknown): void;
    static (value:unknown):boolean;
    /** Returns the primitive value of the specified object. */
    valueOf(): boolean;
    toString(): string;
}

/** An object that represents a number of any kind. All JavaScript numbers are 64-bit floating-point numbers. */
interface NumberConstructor {
    readonly prototype: Number;
    /** The largest number that can be represented in JavaScript. Equal to approximately 1.79E+308. */
    MAX_VALUE: number;
    /** The closest number to zero that can be represented in JavaScript. Equal to approximately 5.00E-324. */
    MIN_VALUE: number;
    /**
     * A value that is not a number.
     * In equality comparisons, NaN does not equal any value, including itself. To test whether a value is equivalent to NaN, use the isNaN function.
     */
    NaN: number;
    /**
     * A value that is less than the largest negative number that can be represented in JavaScript.
     * JavaScript displays NEGATIVE_INFINITY values as -infinity.
     */
    NEGATIVE_INFINITY: number;
    /**
     * A value greater than the largest number that can be represented in JavaScript.
     * JavaScript displays POSITIVE_INFINITY values as infinity.
     */
    POSITIVE_INFINITY: number;
    (value: unknown): number;
    new(value?: unknown): Number;
}

declare var Number: NumberConstructor;

interface Number {
    /**
     * Returns a string containing a number represented in exponential notation.
     * @param fractionDigits Number of digits after the decimal point. Must be in the range 0 - 20, inclusive.
     */
    toExponential(fractionDigits?: number): string;
    /**
     * Returns a string representing a number in fixed-point notation.
     * @param fractionDigits Number of digits after the decimal point. Must be in the range 0 - 20, inclusive.
     */
    toFixed(fractionDigits?: number): string;
    /**
     * Converts a number to a string by using the current or specified locale.
     * @param locales A locale string or array of locale strings that contain one or more language or locale tags. If you include more than one locale string, list them in descending order of priority so that the first entry is the preferred locale. If you omit this parameter, the default locale of the JavaScript runtime is used.
     * @param options An object that contains one or more properties that specify comparison options.
     */
    toLocaleString(locales?: string | Array<string>, options?: Intl$NumberFormatOptions): string;
    /**
     * Returns a string containing a number represented either in exponential or fixed-point notation with a specified number of digits.
     * @param precision Number of significant digits. Must be in the range 1 - 21, inclusive.
     */
    toPrecision(precision?: number): string;
    /**
     * Returns a string representation of an object.
     * @param radix Specifies a radix for converting numeric values to strings. This value is only used for numbers.
     */
    toString(radix?: number): string;
    /** Returns the primitive value of the specified object. */
    valueOf(): number;
}

/** An intrinsic object that provides basic mathematics functionality and constants. */
interface Math {
    /** The mathematical constant e. This is Euler's number, the base of natural logarithms. */
    E: number,
    /** The natural logarithm of 10. */
    LN10: number,
    /** The natural logarithm of 2. */
    LN2: number,
    /** The base-10 logarithm of e. */
    LOG10E: number,
    /** The base-2 logarithm of e. */
    LOG2E: number,
    /** Pi. This is the ratio of the circumference of a circle to its diameter. */
    PI: number,
    /** The square root of 0.5, or, equivalently, one divided by the square root of 2. */
    SQRT1_2: number,
    /** The square root of 2. */
    SQRT2: number,
    /**
     * Returns the absolute value of a number (the value without regard to whether it is positive or negative).
     * For example, the absolute value of -5 is the same as the absolute value of 5.
     * @param x A numeric expression for which the absolute value is needed.
     */
    abs(x: number): number,
    /**
     * Returns the arc cosine (or inverse cosine) of a number.
     * @param x A numeric expression.
     */
    acos(x: number): number,
    /**
     * Returns the arcsine of a number.
     * @param x A numeric expression.
     */
    asin(x: number): number,
    /**
     * Returns the arctangent of a number.
     * @param x A numeric expression for which the arctangent is needed.
     */
    atan(x: number): number,
    /**
     * Returns the angle (in radians) from the X axis to a point.
     * @param y A numeric expression representing the cartesian y-coordinate.
     * @param x A numeric expression representing the cartesian x-coordinate.
     */
    atan2(y: number, x: number): number,
    /**
     * Returns the smallest integer greater than or equal to its numeric argument.
     * @param x A numeric expression.
     */
    ceil(x: number): number,
    /**
     * Returns the cosine of a number.
     * @param x A numeric expression that contains an angle measured in radians.
     */
    cos(x: number): number,
    /**
     * Returns e (the base of natural logarithms) raised to a power.
     * @param x A numeric expression representing the power of e.
     */
    exp(x: number): number,
    /**
     * Returns the greatest integer less than or equal to its numeric argument.
     * @param x A numeric expression.
     */
    floor(x: number): number,
    /**
     * Returns the natural logarithm (base e) of a number.
     * @param x A numeric expression.
     */
    log(x: number): number,
    /**
     * Returns the larger of a set of supplied numeric expressions.
     * @param values Numeric expressions to be evaluated.
     */
    max(...values: Array<number>): number,
    /**
     * Returns the smaller of a set of supplied numeric expressions.
     * @param values Numeric expressions to be evaluated.
     */
    min(...values: Array<number>): number,
    /**
     * Returns the value of a base expression taken to a specified power.
     * @param x The base value of the expression.
     * @param y The exponent value of the expression.
     */
    pow(x: number, y: number): number,
    /** Returns a pseudorandom number between 0 and 1. */
    random(): number,
    /**
     * Returns a supplied numeric expression rounded to the nearest integer.
     * @param x The value to be rounded to the nearest integer.
     */
    round(x: number): number,
    /**
     * Returns the sine of a number.
     * @param x A numeric expression that contains an angle measured in radians.
     */
    sin(x: number): number,
    /**
     * Returns the square root of a number.
     * @param x A numeric expression.
     */
    sqrt(x: number): number,
    /**
     * Returns the tangent of a number.
     * @param x A numeric expression that contains an angle measured in radians.
     */
    tan(x: number): number,
}

declare var Math: Math;


type $ReadOnlyArray<out T> = ReadonlyArray<T>;

/**
 * A class of Array methods and properties that don't mutate the array.
 */
declare class ReadonlyArray<out T> {
    /**
     * Returns a string representation of an array. The elements are converted to string using their toLocalString methods.
     */
    toLocaleString(): string;
    // concat creates a new array
    /**
     * Combines two or more arrays.
     * @param items Additional items to add to the end of array1.
     */
    concat<
      // $FlowFixMe[incompatible-variance]
      S = T
      // $FlowFixMe[incompatible-variance]
    >(...items: Array<ReadonlyArray<S> | S>): Array<T | S>;
    /**
     * Determines whether all the members of an array satisfy the specified test.
     * @param callbackfn A function that accepts up to three arguments. The every method calls
     * the predicate function for each element in the array until the predicate returns a value
     * which is coercible to the Boolean value false, or until the end of the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    every<This>(callbackfn: (this : This, value: T, index: number, array: ReadonlyArray<T>) => unknown, thisArg: This): boolean;
    /**
     * Returns the elements of an array that meet the condition specified in a callback function.
     * @param callbackfn A function that accepts up to three arguments. The filter method calls the predicate function one time for each element in the array.
     */
    filter(callbackfn: typeof Boolean): Array<NonNullable<T>>;
    /**
     * Returns the elements of an array that meet the condition specified in a callback function.
     * @param callbackfn A predicate function that accepts up to three arguments. The filter method calls the predicate function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function. If thisArg is omitted, undefined is used as the this value.
     * @returns An array whose type is specified by the predicate function passed as callbackfn.
     */
    filter<
      // $FlowFixMe[incompatible-variance]
      This, S extends T
    >(callbackfn: (this: This, value: T, index: number, array: ReadonlyArray<T>) => implies value is S, thisArg: This): Array<S>;
    /**
     * Returns the elements of an array that meet the condition specified in a callback function.
     * @param callbackfn A function that accepts up to three arguments. The filter method calls the predicate function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function. If thisArg is omitted, undefined is used as the this value.
     */
    filter<This>(
      callbackfn: (this : This, value: T, index: number, array: ReadonlyArray<T>) => unknown, thisArg : This
      // $FlowFixMe[incompatible-variance]
    ): Array<T>;
    /**
     * Performs the specified action for each element in an array.
     * @param callbackfn  A function that accepts up to three arguments. forEach calls the callbackfn function one time for each element in the array.
     * @param thisArg  An object to which the this keyword can refer in the callbackfn function. If thisArg is omitted, undefined is used as the this value.
     */
    forEach<This>(callbackfn: (this : This, value: T, index: number, array: ReadonlyArray<T>) => unknown, thisArg: This): void;
    /**
     * Returns the index of the first occurrence of a value in an array.
     * @param searchElement The value to locate in the array.
     * @param fromIndex The array index at which to begin the search. If fromIndex is omitted, the search starts at index 0.
     */
    indexOf(
      // $FlowFixMe[incompatible-variance]
      searchElement: T, fromIndex?: number
    ): number;
    /**
     * Adds all the elements of an array separated by the specified separator string.
     * @param separator A string used to separate one element of an array from the next in the resulting String. If omitted, the array elements are separated with a comma.
     */
    join(separator?: string): string;
    /**
     * Returns the index of the last occurrence of a specified value in an array.
     * @param searchElement The value to locate in the array.
     * @param fromIndex The array index at which to begin the search. If fromIndex is omitted, the search starts at the last index in the array.
     */
    lastIndexOf(
      // $FlowFixMe[incompatible-variance]
      searchElement: T, fromIndex?: number
    ): number;
    /**
     * Calls a defined callback function on each element of an array, and returns an array that contains the results.
     * @param callbackfn A function that accepts up to three arguments. The map method calls the callbackfn function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the callbackfn function. If thisArg is omitted, undefined is used as the this value.
     */
    map<U, This>(callbackfn: (this : This, value: T, index: number, array: ReadonlyArray<T>) => U, thisArg: This): Array<U>;
    /**
     * Calls the specified callback function for all the elements in an array. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
     * @param callbackfn A function that accepts up to four arguments. The reduce method calls the callbackfn function one time for each element in the array.
     */
    reduce(
      // $FlowFixMe[incompatible-variance]
      callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: ReadonlyArray<T>) => T,
    ): T;
    /**
     * Calls the specified callback function for all the elements in an array. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
     * @param callbackfn A function that accepts up to four arguments. The reduce method calls the callbackfn function one time for each element in the array.
     * @param initialValue If initialValue is specified, it is used as the initial value to start the accumulation. The first call to the callbackfn function provides this value as an argument instead of an array value.
     */
    reduce<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: ReadonlyArray<T>) => U,
      initialValue: U
    ): U;
    /**
     * Calls the specified callback function for all the elements in an array, in descending order. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
     * @param callbackfn A function that accepts up to four arguments. The reduceRight method calls the callbackfn function one time for each element in the array.
     */
    reduceRight(
      // $FlowFixMe[incompatible-variance]
      callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: ReadonlyArray<T>) => T,
    ): T;
    /**
     * Calls the specified callback function for all the elements in an array, in descending order. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
     * @param callbackfn A function that accepts up to four arguments. The reduceRight method calls the callbackfn function one time for each element in the array.
     * @param initialValue If initialValue is specified, it is used as the initial value to start the accumulation. The first call to the callbackfn function provides this value as an argument instead of an array value.
     */
    reduceRight<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: ReadonlyArray<T>) => U,
      initialValue: U
    ): U;
    /**
     * Returns a section of an array.
     * @param start The beginning of the specified portion of the array.
     * @param end The end of the specified portion of the array. This is exclusive of the element at the index 'end'.
     */
    slice(
      start?: number, end?: number
      // $FlowFixMe[incompatible-variance]
    ): Array<T>;
    /**
     * Determines whether the specified callback function returns true for any element of an array.
     * @param callbackfn A function that accepts up to three arguments. The some method calls
     * the predicate function for each element in the array until the predicate returns a value
     * which is coercible to the Boolean value true, or until the end of the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    some<This>(callbackfn: (this : This, value: T, index: number, array: ReadonlyArray<T>) => unknown, thisArg: This): boolean;

    readonly [key: number]: T;
    /**
     * Gets the length of the array. This is a number one higher than the highest element defined in an array.
     */
    readonly length: number;
}

declare class Array<T> extends ReadonlyArray<T> {
    /**
     * Returns a new JavaScript array with its length property set to that number.
     * (Note: this implies an array of arrayLength empty slots, not slots with actual undefined
     * values. See [sparse arrays](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Indexed_collections#sparse_arrays)).
     */
    constructor(arrayLength?: number): void;
    /**
     * Determines whether all the members of an array satisfy the specified test.
     * @param callbackfn A function that accepts up to three arguments. The every method calls
     * the predicate function for each element in the array until the predicate returns a value
     * which is coercible to the Boolean value false, or until the end of the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    every<This>(callbackfn: (this : This, value: T, index: number, array: Array<T>) => unknown, thisArg: This): boolean;
    /**
     * Returns the elements of an array that meet the condition specified in a callback function.
     * @param callbackfn A function that accepts up to three arguments. The filter method calls the predicate function one time for each element in the array.
     */
    filter(callbackfn: typeof Boolean): Array<NonNullable<T>>;
    /**
     * Returns the elements of an array that meet the condition specified in a callback function.
     * @param callbackfn A predicate function that accepts up to three arguments. The filter method calls the predicate function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function. If thisArg is omitted, undefined is used as the this value.
     * @returns An array whose type is specified by the predicate function passed as callbackfn.
     */
    filter<This, S extends T>(callbackfn: (this: This, value: T, index: number, array: ReadonlyArray<T>) => implies value is S, thisArg: This): Array<S>;
    /**
     * Returns the elements of an array that meet the condition specified in a callback function.
     * @param callbackfn A function that accepts up to three arguments. The filter method calls the predicate function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function. If thisArg is omitted, undefined is used as the this value.
     */
    filter<This>(callbackfn: (this : This, value: T, index: number, array: Array<T>) => unknown, thisArg: This): Array<T>;
    /**
     * Performs the specified action for each element in an array.
     * @param callbackfn  A function that accepts up to three arguments. forEach calls the callbackfn function one time for each element in the array.
     * @param thisArg  An object to which the this keyword can refer in the callbackfn function. If thisArg is omitted, undefined is used as the this value.
     */
    forEach<This>(callbackfn: (this : This, value: T, index: number, array: Array<T>) => unknown, thisArg: This): void;
    /**
     * Calls a defined callback function on each element of an array, and returns an array that contains the results.
     * @param callbackfn A function that accepts up to three arguments. The map method calls the callbackfn function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the callbackfn function. If thisArg is omitted, undefined is used as the this value.
     */
    map<U, This>(callbackfn: (this : This, value: T, index: number, array: Array<T>) => U, thisArg: This): Array<U>;
    /**
     * Removes the last element from an array and returns it.
     */
    pop(): T | void;
    /**
     * Appends new elements to an array, and returns the new length of the array.
     * @param items New elements of the Array.
     */
    push(...items: Array<T>): number;
    /**
     * Calls the specified callback function for all the elements in an array. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
     * @param callbackfn A function that accepts up to four arguments. The reduce method calls the callbackfn function one time for each element in the array.
     */
    reduce(
      callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: Array<T>) => T,
    ): T;
    /**
     * Calls the specified callback function for all the elements in an array. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
     * @param callbackfn A function that accepts up to four arguments. The reduce method calls the callbackfn function one time for each element in the array.
     * @param initialValue If initialValue is specified, it is used as the initial value to start the accumulation. The first call to the callbackfn function provides this value as an argument instead of an array value.
     */
    reduce<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Array<T>) => U,
      initialValue: U
    ): U;
    /**
     * Calls the specified callback function for all the elements in an array, in descending order. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
     * @param callbackfn A function that accepts up to four arguments. The reduceRight method calls the callbackfn function one time for each element in the array.
     */
    reduceRight(
      callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: Array<T>) => T,
    ): T;
    /**
     * Calls the specified callback function for all the elements in an array, in descending order. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
     * @param callbackfn A function that accepts up to four arguments. The reduceRight method calls the callbackfn function one time for each element in the array.
     * @param initialValue If initialValue is specified, it is used as the initial value to start the accumulation. The first call to the callbackfn function provides this value as an argument instead of an array value.
     */
    reduceRight<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Array<T>) => U,
      initialValue: U
    ): U;
    /**
     * Reverses the elements in an Array.
     */
    reverse(): Array<T>;
    /**
     * Removes the first element from an array and returns it.
     */
    shift(): T | void;
    some<This>(callbackfn: (this : This, value: T, index: number, array: Array<T>) => unknown, thisArg: This): boolean;
    sort(compareFn?: (a: T, b: T) => number): Array<T>;
    splice(start: number, deleteCount?: number, ...items: Array<T>): Array<T>;
    unshift(...items: Array<T>): number;


    [key: number]: T;
    /**
     * Gets or sets the length of the array. This is a number one higher than the highest element defined in an array.
     */
    length: number;
    static (arrayLength?: number): Array<any>;
    static <T>(arrayLength: number): Array<T>;
    static <T>(...items: Array<T>): Array<T>;
    static isArray(obj: unknown): boolean;
    /**
     * Creates an array from an iterable object.
     * @param iterable An iterable object to convert to an array.
     */
    static from<T>(iterable: Iterable<T> | ArrayLike<T>): Array<T>;
    /**
     * Creates an array from an iterable object.
     * @param iterable An iterable object to convert to an array.
     * @param mapfn A mapping function to call on every element of the array.
     * @param thisArg Value of 'this' used to invoke the mapfn.
     */
    static from<T, U>(iterable: Iterable<T> | ArrayLike<T>, mapfn: (v: T, k: number) => U, thisArg?: any): Array<U>;
    /**
     * Creates an array from a string, by splitting it into its individual Unicode code points.
     * @param str A string to convert into an Array.
     * @param mapFn A mapping function to call on every element of the string.
     * @param thisArg Value of 'this' used to invoke the mapfn.
     */
    static from<A, This>(str: string, mapFn: (this: This, elem: string, index: number) => A, thisArg: This): Array<A>;
    /**
     * Creates an array from a string, by splitting it into its individual Unicode code points.
     * @param str A string to convert into an Array.
     */
    static from(str: string): Array<string>;
    /**
     * Returns a new array from a set of elements.
     * @param values A set of elements to include in the new array object.
     */
    static of<T>(...values: Array<T>): Array<T>;
}

interface ArrayLike<out T> {
  readonly [indexer: number]: T;
  readonly length: number;
}
type $ArrayLike<T> = interface {readonly [indexer: number]: T; @@iterator(): IteratorObject<T>; readonly length: number;};

interface RegExpMatchArray extends Array<string> {
    index: number;
    input: string;
}
type RegExp$matchResult = RegExpMatchArray;

interface TaggedTemplateLiteralArray extends ReadonlyArray<string> {
  readonly raw: ReadonlyArray<string>;
}

declare type Intl$CollatorOptions = {
  localeMatcher?: 'lookup' | 'best fit',
  usage?: 'sort' | 'search',
  sensitivity?: 'base' | 'accent' | 'case' | 'variant',
  ignorePunctuation?: boolean,
  numeric?: boolean,
  caseFirst?: 'upper' | 'lower' | 'false',
  ...
}

declare type Intl$DateTimeFormatOptions = {
  localeMatcher?: 'lookup' | 'best fit',
  timeZone?: string,
  hour12?: boolean,
  formatMatcher?: 'basic' | 'best fit',
  weekday?: 'narrow' | 'short' | 'long',
  era?: 'narrow' | 'short' | 'long',
  year?: 'numeric' | '2-digit',
  month?: 'numeric' | '2-digit' | 'narrow' | 'short' | 'long',
  day?: 'numeric' | '2-digit',
  hour?: 'numeric' | '2-digit',
  minute?: 'numeric' | '2-digit',
  second?: 'numeric' | '2-digit',
  timeZoneName?: 'short' | 'long',
  ...
}

declare type Intl$NumberFormatOptions = {
  localeMatcher?: 'lookup' | 'best fit',
  style?: 'decimal' | 'currency' | 'percent' | 'unit',
  currency?: string,
  currencyDisplay?: 'symbol' | 'code' | 'name' | 'narrowSymbol',
  useGrouping?: boolean,
  minimumIntegerDigits?: number,
  minimumFractionDigits?: number,
  maximumFractionDigits?: number,
  minimumSignificantDigits?: number,
  maximumSignificantDigits?: number,
  ...
}

/**
 * Allows manipulation and formatting of text strings and determination and location of substrings within strings.
 */
interface String {
    /**
     * Returns the character at the specified index.
     * @param pos The zero-based index of the desired character.
     */
    charAt(pos: number): string;
    /**
     * Returns the Unicode value of the character at the specified location.
     * @param index The zero-based index of the desired character. If there is no character at the specified index, NaN is returned.
     */
    charCodeAt(index: number): number;
    /**
     * Returns a string that contains the concatenation of two or more strings.
     * @param strings The strings to append to the end of the string.
     */
    concat(...strings: Array<string>): string;
    /**
     * Returns the position of the first occurrence of a substring.
     * @param searchString The substring to search for in the string
     * @param position The index at which to begin searching the String object. If omitted, search starts at the beginning of the string.
     */
    indexOf(searchString: string, position?: number): number;
    /**
     * Returns the last occurrence of a substring in the string.
     * @param searchString The substring to search for.
     * @param position The index at which to begin searching. If omitted, the search begins at the end of the string.
     */
    lastIndexOf(searchString: string, position?: number): number;
    /**
     * Determines whether two strings are equivalent in the current or specified locale.
     * @param that String to compare to target string
     * @param locales A locale string or array of locale strings that contain one or more language or locale tags. If you include more than one locale string, list them in descending order of priority so that the first entry is the preferred locale. If you omit this parameter, the default locale of the JavaScript runtime is used. This parameter must conform to BCP 47 standards; see the Intl.Collator object for details.
     * @param options An object that contains one or more properties that specify comparison options. see the Intl.Collator object for details.
     */
    localeCompare(that: string, locales?: string | Array<string>, options?: Intl$CollatorOptions): number;
    /**
     * Matches a string with a regular expression, and returns an array containing the results of that search.
     * @param regexp A variable name or string literal containing the regular expression pattern and flags.
     */
    match(regexp: string | RegExp): RegExpMatchArray | null;
    /**
     * Replaces text in a string, using a regular expression or search string.
     * @param searchValue A string to search for.
     * @param replaceValue A string containing the text to replace for every successful match of searchValue in this string or a function that returns the replacement text.
     */
    replace(searchValue: string | RegExp, replaceValue: string | (substring: string, ...args: Array<any>) => string): string;
    /**
     * Finds the first substring match in a regular expression search.
     * @param regexp The regular expression pattern and applicable flags.
     */
    search(regexp: string | RegExp): number;
    /**
     * Returns a section of a string.
     * @param start The index to the beginning of the specified portion of stringObj.
     * @param end The index to the end of the specified portion of stringObj. The substring includes the characters up to, but not including, the character indicated by end.
     * If this value is not specified, the substring continues to the end of stringObj.
     */
    slice(start?: number, end?: number): string;
    /**
     * Split a string into substrings using the specified separator and return them as an array.
     * @param separator A string that identifies character or characters to use in separating the string. If omitted, a single-element array containing the entire string is returned.
     * @param limit A value used to limit the number of elements returned in the array.
     */
    split(separator?: string | RegExp, limit?: number): Array<string>;
    /**
     * Gets a substring beginning at the specified location and having the specified length.
     * @param from The starting position of the desired substring. The index of the first character in the string is zero.
     * @param length The number of characters to include in the returned substring.
     */
    substr(from: number, length?: number): string;
    /**
     * Returns the substring at the specified location within a String object.
     * @param start The zero-based index number indicating the beginning of the substring.
     * @param end Zero-based index number indicating the end of the substring. The substring includes the characters up to, but not including, the character indicated by end.
     * If end is omitted, the characters from start through the end of the original string are returned.
     */
    substring(start: number, end?: number): string;
    /** Converts all alphabetic characters to lowercase, taking into account the host environment's current locale. */
    toLocaleLowerCase(locale?: string | Array<string>): string;
    /** Returns a string where all alphabetic characters have been converted to uppercase, taking into account the host environment's current locale. */
    toLocaleUpperCase(locale?: string | Array<string>): string;
    /** Converts all the alphabetic characters in a string to lowercase. */
    toLowerCase(): string;
    /** Converts all the alphabetic characters in a string to uppercase. */
    toUpperCase(): string;
    /** Removes the leading and trailing white space and line terminator characters from a string. */
    trim(): string;
    /** Returns the primitive value of the specified object. */
    valueOf(): string;
    /** Returns a string representation of a string. */
    toString(): string;
    /** Returns the length of a String object. */
    length: number;
    [key: number]: string;
}

interface StringConstructor {
    readonly prototype: String;
    new(value?: unknown): String;
    (value: unknown): string;
    fromCharCode(...codes: Array<number>): string;
}

declare var String: StringConstructor;

interface RegExpConstructor {
    readonly prototype: RegExp;
    (pattern: string | RegExp, flags?: string): RegExp;
    escape(string: string): string;
    new(pattern: string | RegExp, flags?: string): RegExp;
}

declare var RegExp: RegExpConstructor;

interface RegExp {
    compile(): RegExp;
    /**
     * Executes a search on a string using a regular expression pattern, and returns an array containing the results of that search.
     * @param string The String object or string literal on which to perform the search.
     */
    exec(string: string): RegExpMatchArray | null;
    /** Returns a Boolean value indicating the state of the global flag (g) used with a regular expression. Default is false. Read-only. */
    global: boolean;
    /** Returns a Boolean value indicating the state of the ignoreCase flag (i) used with a regular expression. Default is false. Read-only. */
    ignoreCase: boolean;
    lastIndex: number;
    /** Returns a Boolean value indicating the state of the multiline flag (m) used with a regular expression. Default is false. Read-only. */
    multiline: boolean;
    /** Returns a copy of the text of the regular expression pattern. Read-only. The regExp argument is a Regular expression object. It can be a variable name or a literal. */
    source: string;
    /**
     * Returns a Boolean value that indicates whether or not a pattern exists in a searched string.
     * @param string String on which to perform the search.
     */
    test(string: string): boolean;
    toString(): string;
}

/** Enables basic storage and retrieval of dates and times. */
interface Date {
    /** Gets the day-of-the-month, using local time. */
    getDate(): number;
    /** Gets the day of the week, using local time. */
    getDay(): number;
    /** Gets the year, using local time. */
    getFullYear(): number;
    /** Gets the hours in a date, using local time. */
    getHours(): number;
    /** Gets the milliseconds of a Date, using local time. */
    getMilliseconds(): number;
    /** Gets the minutes of a Date object, using local time. */
    getMinutes(): number;
    /** Gets the month, using local time. */
    getMonth(): number;
    /** Gets the seconds of a Date object, using local time. */
    getSeconds(): number;
    /** Gets the time value in milliseconds. */
    getTime(): number;
    /** Gets the difference in minutes between the time on the local computer and Universal Coordinated Time (UTC). */
    getTimezoneOffset(): number;
    /** Gets the day-of-the-month, using Universal Coordinated Time (UTC). */
    getUTCDate(): number;
    /** Gets the day of the week using Universal Coordinated Time (UTC). */
    getUTCDay(): number;
    /** Gets the year using Universal Coordinated Time (UTC). */
    getUTCFullYear(): number;
    /** Gets the hours value in a Date object using Universal Coordinated Time (UTC). */
    getUTCHours(): number;
    /** Gets the milliseconds of a Date object using Universal Coordinated Time (UTC). */
    getUTCMilliseconds(): number;
    /** Gets the minutes of a Date object using Universal Coordinated Time (UTC). */
    getUTCMinutes(): number;
    /** Gets the month of a Date object using Universal Coordinated Time (UTC). */
    getUTCMonth(): number;
    /** Gets the seconds of a Date object using Universal Coordinated Time (UTC). */
    getUTCSeconds(): number;
    /**
     * Sets the numeric day-of-the-month value of the Date object using local time.
     * @param date A numeric value equal to the day of the month.
     */
    setDate(date: number): number;
    /**
     * Sets the year of the Date object using local time.
     * @param year A numeric value for the year.
     * @param month A zero-based numeric value for the month (0 for January, 11 for December). Must be specified if numDate is specified.
     * @param date A numeric value equal for the day of the month.
     */
    setFullYear(year: number, month?: number, date?: number): number;
    /**
     * Sets the hour value in the Date object using local time.
     * @param hours A numeric value equal to the hours value.
     * @param min A numeric value equal to the minutes value.
     * @param sec A numeric value equal to the seconds value.
     * @param ms A numeric value equal to the milliseconds value.
     */
    setHours(hours: number, min?: number, sec?: number, ms?: number): number;
    /**
     * Sets the milliseconds value in the Date object using local time.
     * @param ms A numeric value equal to the millisecond value.
     */
    setMilliseconds(ms: number): number;
    /**
     * Sets the minutes value in the Date object using local time.
     * @param min A numeric value equal to the minutes value.
     * @param sec A numeric value equal to the seconds value.
     * @param ms A numeric value equal to the milliseconds value.
     */
    setMinutes(min: number, sec?: number, ms?: number): number;
    /**
     * Sets the month value in the Date object using local time.
     * @param month A numeric value equal to the month. The value for January is 0, and other month values follow consecutively.
     * @param date A numeric value representing the day of the month. If this value is not supplied, the value from a call to the getDate method is used.
     */
    setMonth(month: number, date?: number): number;
    /**
     * Sets the seconds value in the Date object using local time.
     * @param sec A numeric value equal to the seconds value.
     * @param ms A numeric value equal to the milliseconds value.
     */
    setSeconds(sec: number, ms?: number): number;
    /**
     * Sets the date and time value in the Date object.
     * @param time A numeric value representing the number of elapsed milliseconds since midnight, January 1, 1970 GMT.
     */
    setTime(time: number): number;
    /**
     * Sets the numeric day of the month in the Date object using Universal Coordinated Time (UTC).
     * @param date A numeric value equal to the day of the month.
     */
    setUTCDate(date: number): number;
    /**
     * Sets the year value in the Date object using Universal Coordinated Time (UTC).
     * @param year A numeric value equal to the year.
     * @param month A numeric value equal to the month. The value for January is 0, and other month values follow consecutively. Must be supplied if numDate is supplied.
     * @param date A numeric value equal to the day of the month.
     */
    setUTCFullYear(year: number, month?: number, date?: number): number;
    /**
     * Sets the hours value in the Date object using Universal Coordinated Time (UTC).
     * @param hours A numeric value equal to the hours value.
     * @param min A numeric value equal to the minutes value.
     * @param sec A numeric value equal to the seconds value.
     * @param ms A numeric value equal to the milliseconds value.
     */
    setUTCHours(hours: number, min?: number, sec?: number, ms?: number): number;
    /**
     * Sets the milliseconds value in the Date object using Universal Coordinated Time (UTC).
     * @param ms A numeric value equal to the millisecond value.
     */
    setUTCMilliseconds(ms: number): number;
    /**
     * Sets the minutes value in the Date object using Universal Coordinated Time (UTC).
     * @param min A numeric value equal to the minutes value.
     * @param sec A numeric value equal to the seconds value.
     * @param ms A numeric value equal to the milliseconds value.
     */
    setUTCMinutes(min: number, sec?: number, ms?: number): number;
    /**
     * Sets the month value in the Date object using Universal Coordinated Time (UTC).
     * @param month A numeric value equal to the month. The value for January is 0, and other month values follow consecutively.
     * @param date A numeric value representing the day of the month. If it is not supplied, the value from a call to the getUTCDate method is used.
     */
    setUTCMonth(month: number, date?: number): number;
    /**
     * Sets the seconds value in the Date object using Universal Coordinated Time (UTC).
     * @param sec A numeric value equal to the seconds value.
     * @param ms A numeric value equal to the milliseconds value.
     */
    setUTCSeconds(sec: number, ms?: number): number;
    /** Returns a date as a string value. */
    toDateString(): string;
    /** Returns a date as a string value in ISO format. */
    toISOString(): string;
    /** Used by the JSON.stringify method to enable the transformation of an object's data for JavaScript Object Notation (JSON) serialization. */
    toJSON(key?: unknown): string;
    /**
     * Converts a date to a string by using the current or specified locale.
     * @param locales A locale string or array of locale strings that contain one or more language or locale tags. If you include more than one locale string, list them in descending order of priority so that the first entry is the preferred locale. If you omit this parameter, the default locale of the JavaScript runtime is used.
     * @param options An object that contains one or more properties that specify comparison options.
     */
    toLocaleDateString(locales?: string | Array<string>, options?: Intl$DateTimeFormatOptions): string;
    /**
     * Converts a date and time to a string by using the current or specified locale.
     * @param locales A locale string or array of locale strings that contain one or more language or locale tags. If you include more than one locale string, list them in descending order of priority so that the first entry is the preferred locale. If you omit this parameter, the default locale of the JavaScript runtime is used.
     * @param options An object that contains one or more properties that specify comparison options.
     */
    toLocaleString(locales?: string | Array<string>, options?: Intl$DateTimeFormatOptions): string;
    /**
     * Converts a time to a string by using the current or specified locale.
     * @param locales A locale string or array of locale strings that contain one or more language or locale tags. If you include more than one locale string, list them in descending order of priority so that the first entry is the preferred locale. If you omit this parameter, the default locale of the JavaScript runtime is used.
     * @param options An object that contains one or more properties that specify comparison options.
     */
    toLocaleTimeString(locales?: string | Array<string>, options?: Intl$DateTimeFormatOptions): string;
    /** Returns a time as a string value. */
    toTimeString(): string;
    /** Returns a date converted to a string using Universal Coordinated Time (UTC). */
    toUTCString(): string;
    /** Returns the stored time value in milliseconds since midnight, January 1, 1970 UTC. */
    valueOf(): number;
}

interface DateConstructor {
    readonly prototype: Date;
    new(): Date;
    new(value: number | Date | string): Date;
    new(year: number, month: number, day?: number, hour?: number, minute?: number, second?: number, millisecond?: number): Date;
    (): string;
    now(): number;
    /**
     * Parses a string containing a date, and returns the number of milliseconds between that date and midnight, January 1, 1970.
     * @param s A date string
     */
    parse(s: string): number;
    /**
     * Returns the number of milliseconds between midnight, January 1, 1970 Universal Coordinated Time (UTC) (or GMT) and the specified date.
     * @param year The full year designation is required for cross-century date accuracy. If year is between 0 and 99 is used, then year is assumed to be 1900 + year.
     * @param month The month as a number between 0 and 11 (January to December).
     * @param date The date as a number between 1 and 31.
     * @param hours Must be supplied if minutes is supplied. A number from 0 to 23 (midnight to 11pm) that specifies the hour.
     * @param minutes Must be supplied if seconds is supplied. A number from 0 to 59 that specifies the minutes.
     * @param seconds Must be supplied if milliseconds is supplied. A number from 0 to 59 that specifies the seconds.
     * @param ms A number from 0 to 999 that specifies the milliseconds.
     */
    UTC(year: number, month: number, date?: number, hours?: number, minutes?: number, seconds?: number, ms?: number): number;
}

declare var Date: DateConstructor;

interface Error {
    name: string;
    message: string;
    stack: string;
    toString(): string;

    // note: microsoft only
    description?: string;
    number?: number;

    // note: mozilla only
    fileName?: string;
    lineNumber?: number;
    columnNumber?: number;

}

interface ErrorConstructor {
    readonly prototype: Error;
    (message?: string, options?: {cause: unknown, ...}): Error;
    new(message?: unknown, options?: {cause: unknown, ...}): Error;

    // note: v8 only (node/chrome)
    captureStackTrace(target: interface { [any] : any }, constructor?: any): void;

    stackTraceLimit: number;
    prepareStackTrace: (err: Error, stack: CallSite[]) => unknown;
}

declare var Error: ErrorConstructor;

interface EvalError extends Error {}
interface EvalErrorConstructor extends ErrorConstructor {
    readonly prototype: EvalError;
    (message?: string): Error;
    new(message?: unknown, options?: {cause: unknown, ...}): EvalError;
}
declare var EvalError: EvalErrorConstructor;

interface RangeError extends Error {}
interface RangeErrorConstructor extends ErrorConstructor {
    readonly prototype: RangeError;
    (message?: string): Error;
    new(message?: unknown, options?: {cause: unknown, ...}): RangeError;
}
declare var RangeError: RangeErrorConstructor;

interface ReferenceError extends Error {}
interface ReferenceErrorConstructor extends ErrorConstructor {
    readonly prototype: ReferenceError;
    (message?: string): Error;
    new(message?: unknown, options?: {cause: unknown, ...}): ReferenceError;
}
declare var ReferenceError: ReferenceErrorConstructor;

interface SyntaxError extends Error {}
interface SyntaxErrorConstructor extends ErrorConstructor {
    readonly prototype: SyntaxError;
    (message?: string): Error;
    new(message?: unknown, options?: {cause: unknown, ...}): SyntaxError;
}
declare var SyntaxError: SyntaxErrorConstructor;

interface TypeError extends Error {}
interface TypeErrorConstructor extends ErrorConstructor {
    readonly prototype: TypeError;
    (message?: string): Error;
    new(message?: unknown, options?: {cause: unknown, ...}): TypeError;
}
declare var TypeError: TypeErrorConstructor;

interface URIError extends Error {}
interface URIErrorConstructor extends ErrorConstructor {
    readonly prototype: URIError;
    (message?: string): Error;
    new(message?: unknown, options?: {cause: unknown, ...}): URIError;
}
declare var URIError: URIErrorConstructor;

/**
 * An intrinsic object that provides functions to convert JavaScript values to and from the JavaScript Object Notation (JSON) format.
 */
interface JSON {
    /**
     * Converts a JavaScript Object Notation (JSON) string into an object.
     * @param text A valid JSON string.
     * @param reviver A function that transforms the results. This function is called for each member of the object.
     * If a member contains nested objects, the nested objects are transformed before the parent object is.
     */
    readonly parse: (text: string, reviver?: (key: any, value: any) => any) => any,
    /**
     * Converts a JavaScript value to a JavaScript Object Notation (JSON) string.
     * @param value A JavaScript value, usually an object or array, to be converted.
     * @param replacer A function that transforms the results or an array of strings and numbers that acts as a approved list for selecting the object properties that will be stringified.
     * @param space Adds indentation, white space, and line break characters to the return-value JSON text to make it easier to read.
     */
    readonly stringify: ((
        value: null | string | number | boolean | interface {} | ReadonlyArray<unknown>,
        replacer?: ?((key: string, value: any) => any) | Array<any>,
        space?: string | number
      ) => string) &
      (
        value: unknown,
        replacer?: ?((key: string, value: any) => any) | Array<any>,
        space?: string | number
      ) => string | void,
}

declare var JSON: JSON;

// Technically this should only allow non-registered symbols (i.e. symbols
// created via `Symbol()`, NOT symbols created via `Symbol.for()`), but
// Flow doesn't currently differentiate between registered and non-registered
// symbols.
// https://fb.workplace.com/groups/flow/posts/32817878791167322
type WeaklyReferenceable = interface {} | ReadonlyArray<unknown> | symbol;

/* Binary data */

interface ArrayBuffer {
    readonly byteLength: number;
    slice(begin?: number, end?: number): ArrayBuffer;
}

interface ArrayBufferConstructor {
    readonly prototype: ArrayBuffer;
    new(byteLength: number): ArrayBuffer;
    isView(arg: unknown): boolean;
}

declare var ArrayBuffer: ArrayBufferConstructor;

type ArrayBufferLike = ArrayBuffer | SharedArrayBuffer;

// This is a helper type to simplify the specification, it isn't an interface
// and there are no objects implementing it.
// https://developer.mozilla.org/en-US/docs/Web/API/ArrayBufferView
type $ArrayBufferView<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> =
  | $TypedArray<TArrayBuffer>
  | DataView<TArrayBuffer>;

type $TypedArrayNumber<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> =
  | Int8Array<TArrayBuffer>
  | Uint8Array<TArrayBuffer>
  | Uint8ClampedArray<TArrayBuffer>
  | Int16Array<TArrayBuffer>
  | Uint16Array<TArrayBuffer>
  | Int32Array<TArrayBuffer>
  | Uint32Array<TArrayBuffer>
  | Float16Array<TArrayBuffer>
  | Float32Array<TArrayBuffer>
  | Float64Array<TArrayBuffer>;

type $TypedArray<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> =
  | $TypedArrayNumber<TArrayBuffer>
  | BigInt64Array<TArrayBuffer>
  | BigUint64Array<TArrayBuffer>;

// The TypedArray intrinsic object is a constructor function, but does not have
// a global name or appear as a property of the global object.
// http://www.ecma-international.org/ecma-262/6.0/#sec-%typedarray%-intrinsic-object
interface $TypedArrayInternal<
  T extends number | bigint,
  out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike,
  out TArrayResult = $TypedArray<TArrayBuffer>,
> {
    [index: number]: T;

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
    every<This>(callback: (this : This, value: T, index: number, array: this) => unknown, thisArg: This): boolean;
    /**
     * Returns the this object after filling the section identified by start and end with value
     * @param value value to fill array section with
     * @param start index to start filling the array at. If start is negative, it is treated as
     * length+start where length is the length of the array.
     * @param end index to stop filling the array at. If end is negative, it is treated as
     * length+end.
     */
    fill(value: T, start?: number, end?: number): this;
    /**
     * Returns the elements of an array that meet the condition specified in a callback function.
     * @param callback A function that accepts up to three arguments. The filter method calls
     * the predicate function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    filter<This>(callback: (this : This, value: T, index: number, array: this) => unknown, thisArg: This): TArrayResult;
    /**
     * Returns the value of the first element in the array where predicate is true, and undefined
     * otherwise.
     * @param callback find calls predicate once for each element of the array, in ascending
     * order, until it finds one where predicate returns true. If such an element is found, find
     * immediately returns that element value. Otherwise, find returns undefined.
     * @param thisArg If provided, it will be used as the this value for each invocation of
     * predicate. If it is not provided, undefined is used instead.
     */
    find<This> (callback: (this : This, value: T, index: number, array: this) => unknown, thisArg: This): T | void;
    /**
     * Returns the index of the first element in the array where predicate is true, and -1
     * otherwise.
     * @param callback find calls predicate once for each element of the array, in ascending
     * order, until it finds one where predicate returns true. If such an element is found,
     * findIndex immediately returns that element index. Otherwise, findIndex returns -1.
     * @param thisArg If provided, it will be used as the this value for each invocation of
     * predicate. If it is not provided, undefined is used instead.
     */
    findIndex<This>(callback: (this : This, value: T, index: number, array: this) => unknown, thisArg: This): number;
    /**
     * Performs the specified action for each element in an array.
     * @param callback  A function that accepts up to three arguments. forEach calls the
     * callbackfn function one time for each element in the array.
     * @param thisArg  An object to which the this keyword can refer in the callbackfn function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    forEach<This>(callback: (this : This, value: T, index: number, array: this) => unknown, thisArg: This): void;
    /**
     * Returns the index of the first occurrence of a value in an array.
     * @param searchElement The value to locate in the array.
     * @param fromIndex The array index at which to begin the search. If fromIndex is omitted, the
     *  search starts at index 0.
     */
    indexOf(searchElement: T, fromIndex?: number): number; // -1 if not present
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
    lastIndexOf(searchElement: T, fromIndex?: number): number; // -1 if not present
    /**
     * Calls a defined callback function on each element of an array, and returns an array that
     * contains the results.
     * @param callback A function that accepts up to three arguments. The map method calls the
     * callbackfn function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the callbackfn function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    map<This>(callback: (this : This, currentValue: T, index: number, array: this) => T, thisArg : This): TArrayResult;
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
      callback: (previousValue: T, currentValue: T, index: number, array: this) => T,
      initialValue: void
    ): T;
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
      callback: (previousValue: U, currentValue: T, index: number, array: this) => U,
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
      callback: (previousValue: T, currentValue: T, index: number, array: this) => T,
      initialValue: void
    ): T;
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
      callback: (previousValue: U, currentValue: T, index: number, array: this) => U,
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
    set(array: ArrayLike<T>, offset?: number): void;
    /**
     * Returns a section of an array.
     * @param start The beginning of the specified portion of the array.
     * @param end The end of the specified portion of the array. This is exclusive of the element at the index 'end'.
     */
    slice(begin?: number, end?: number): TArrayResult;
    /**
     * Determines whether the specified callback function returns true for any element of an array.
     * @param callback A function that accepts up to three arguments. The some method calls
     * the predicate function for each element in the array until the predicate returns a value
     * which is coercible to the Boolean value true, or until the end of the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    some<This>(callback: (this : This, value: T, index: number, array: this) => unknown, thisArg: This): boolean;
    /**
     * Sorts an array.
     * @param compareFn Function used to determine the order of the elements. It is expected to return
     * a negative value if first argument is less than second argument, zero if they're equal and a positive
     * value otherwise. If omitted, the elements are sorted in ascending, ASCII character order.
     * ```ts
     * [11,2,22,1].sort((a, b) => a - b)
     * ```
     */
    sort(compare?: (a: T, b: T) => number): this;
    /**
     * Gets a new Int8Array view of the ArrayBuffer store for this array, referencing the elements
     * at begin, inclusive, up to end, exclusive.
     * @param begin The index of the beginning of the array.
     * @param end The index of the end of the array.
     */
    subarray(begin?: number, end?: number): this;
}

/**
 * A typed array of 8-bit integer values. The contents are initialized to 0. If the requested
 * number of bytes could not be allocated an exception is raised.
 */
interface Int8Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike>
  extends $TypedArrayInternal<number, TArrayBuffer, Int8Array<ArrayBuffer>> {}
/**
 * A typed array of 8-bit unsigned integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
interface Uint8Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike>
  extends $TypedArrayInternal<number, TArrayBuffer, Uint8Array<ArrayBuffer>> {}
/**
 * A typed array of 8-bit unsigned integer (clamped) values. The contents are initialized to 0.
 * If the requested number of bytes could not be allocated an exception is raised.
 */
interface Uint8ClampedArray<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike>
  extends $TypedArrayInternal<number, TArrayBuffer, Uint8ClampedArray<ArrayBuffer>> {}
/**
 * A typed array of 16-bit signed integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
interface Int16Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike>
  extends $TypedArrayInternal<number, TArrayBuffer, Int16Array<ArrayBuffer>> {}
/**
 * A typed array of 16-bit unsigned integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
interface Uint16Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike>
  extends $TypedArrayInternal<number, TArrayBuffer, Uint16Array<ArrayBuffer>> {}
/**
 * A typed array of 32-bit signed integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
interface Int32Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike>
  extends $TypedArrayInternal<number, TArrayBuffer, Int32Array<ArrayBuffer>> {}
/**
 * A typed array of 32-bit unsigned integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
interface Uint32Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike>
  extends $TypedArrayInternal<number, TArrayBuffer, Uint32Array<ArrayBuffer>> {}
/**
 * A typed array of 32-bit float values. The contents are initialized to 0. If the requested number
 * of bytes could not be allocated an exception is raised.
 */
interface Float32Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike>
  extends $TypedArrayInternal<number, TArrayBuffer, Float32Array<ArrayBuffer>> {}
/**
 * A typed array of 64-bit float values. The contents are initialized to 0. If the requested
 * number of bytes could not be allocated an exception is raised.
 */
interface Float64Array<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike>
  extends $TypedArrayInternal<number, TArrayBuffer, Float64Array<ArrayBuffer>> {}

interface $TypedArrayConstructor<T, out TPrototype, out TArray> {
    readonly prototype: TPrototype;
    readonly BYTES_PER_ELEMENT: number;
    from<This = void>(iterable: ArrayLike<T> | Iterable<T>, mapFn?: (this : This, element: T) => T, thisArg?: This): TArray;
    of(...values: T[]): TArray;
}

interface Int8ArrayConstructor extends $TypedArrayConstructor<number, Int8Array<ArrayBufferLike>, Int8Array<ArrayBuffer>> {
    new(length: number): Int8Array<ArrayBuffer>;
    new(array: ArrayLike<number>): Int8Array<ArrayBuffer>;
    new<TArrayBuffer extends ArrayBufferLike = ArrayBuffer>(buffer: TArrayBuffer, byteOffset?: number, length?: number): Int8Array<TArrayBuffer>;
    new(buffer: ArrayBuffer, byteOffset?: number, length?: number): Int8Array<ArrayBuffer>;
    new(array: ArrayLike<number> | ArrayBuffer): Int8Array<ArrayBuffer>;
    new(elements: Iterable<number>): Int8Array<ArrayBuffer>;
}
interface Uint8ArrayConstructor extends $TypedArrayConstructor<number, Uint8Array<ArrayBufferLike>, Uint8Array<ArrayBuffer>> {
    new(length: number): Uint8Array<ArrayBuffer>;
    new(array: ArrayLike<number>): Uint8Array<ArrayBuffer>;
    new<TArrayBuffer extends ArrayBufferLike = ArrayBuffer>(buffer: TArrayBuffer, byteOffset?: number, length?: number): Uint8Array<TArrayBuffer>;
    new(buffer: ArrayBuffer, byteOffset?: number, length?: number): Uint8Array<ArrayBuffer>;
    new(array: ArrayLike<number> | ArrayBuffer): Uint8Array<ArrayBuffer>;
    new(elements: Iterable<number>): Uint8Array<ArrayBuffer>;
}
interface Uint8ClampedArrayConstructor extends $TypedArrayConstructor<number, Uint8ClampedArray<ArrayBufferLike>, Uint8ClampedArray<ArrayBuffer>> {
    new(length: number): Uint8ClampedArray<ArrayBuffer>;
    new(array: ArrayLike<number>): Uint8ClampedArray<ArrayBuffer>;
    new<TArrayBuffer extends ArrayBufferLike = ArrayBuffer>(buffer: TArrayBuffer, byteOffset?: number, length?: number): Uint8ClampedArray<TArrayBuffer>;
    new(buffer: ArrayBuffer, byteOffset?: number, length?: number): Uint8ClampedArray<ArrayBuffer>;
    new(array: ArrayLike<number> | ArrayBuffer): Uint8ClampedArray<ArrayBuffer>;
    new(elements: Iterable<number>): Uint8ClampedArray<ArrayBuffer>;
}
interface Int16ArrayConstructor extends $TypedArrayConstructor<number, Int16Array<ArrayBufferLike>, Int16Array<ArrayBuffer>> {
    new(length: number): Int16Array<ArrayBuffer>;
    new(array: ArrayLike<number>): Int16Array<ArrayBuffer>;
    new<TArrayBuffer extends ArrayBufferLike = ArrayBuffer>(buffer: TArrayBuffer, byteOffset?: number, length?: number): Int16Array<TArrayBuffer>;
    new(buffer: ArrayBuffer, byteOffset?: number, length?: number): Int16Array<ArrayBuffer>;
    new(array: ArrayLike<number> | ArrayBuffer): Int16Array<ArrayBuffer>;
    new(elements: Iterable<number>): Int16Array<ArrayBuffer>;
}
interface Uint16ArrayConstructor extends $TypedArrayConstructor<number, Uint16Array<ArrayBufferLike>, Uint16Array<ArrayBuffer>> {
    new(length: number): Uint16Array<ArrayBuffer>;
    new(array: ArrayLike<number>): Uint16Array<ArrayBuffer>;
    new<TArrayBuffer extends ArrayBufferLike = ArrayBuffer>(buffer: TArrayBuffer, byteOffset?: number, length?: number): Uint16Array<TArrayBuffer>;
    new(buffer: ArrayBuffer, byteOffset?: number, length?: number): Uint16Array<ArrayBuffer>;
    new(array: ArrayLike<number> | ArrayBuffer): Uint16Array<ArrayBuffer>;
    new(elements: Iterable<number>): Uint16Array<ArrayBuffer>;
}
interface Int32ArrayConstructor extends $TypedArrayConstructor<number, Int32Array<ArrayBufferLike>, Int32Array<ArrayBuffer>> {
    new(length: number): Int32Array<ArrayBuffer>;
    new(array: ArrayLike<number>): Int32Array<ArrayBuffer>;
    new<TArrayBuffer extends ArrayBufferLike = ArrayBuffer>(buffer: TArrayBuffer, byteOffset?: number, length?: number): Int32Array<TArrayBuffer>;
    new(buffer: ArrayBuffer, byteOffset?: number, length?: number): Int32Array<ArrayBuffer>;
    new(array: ArrayLike<number> | ArrayBuffer): Int32Array<ArrayBuffer>;
    new(elements: Iterable<number>): Int32Array<ArrayBuffer>;
}
interface Uint32ArrayConstructor extends $TypedArrayConstructor<number, Uint32Array<ArrayBufferLike>, Uint32Array<ArrayBuffer>> {
    new(length: number): Uint32Array<ArrayBuffer>;
    new(array: ArrayLike<number>): Uint32Array<ArrayBuffer>;
    new<TArrayBuffer extends ArrayBufferLike = ArrayBuffer>(buffer: TArrayBuffer, byteOffset?: number, length?: number): Uint32Array<TArrayBuffer>;
    new(buffer: ArrayBuffer, byteOffset?: number, length?: number): Uint32Array<ArrayBuffer>;
    new(array: ArrayLike<number> | ArrayBuffer): Uint32Array<ArrayBuffer>;
    new(elements: Iterable<number>): Uint32Array<ArrayBuffer>;
}
interface Float32ArrayConstructor extends $TypedArrayConstructor<number, Float32Array<ArrayBufferLike>, Float32Array<ArrayBuffer>> {
    new(length: number): Float32Array<ArrayBuffer>;
    new(array: ArrayLike<number>): Float32Array<ArrayBuffer>;
    new<TArrayBuffer extends ArrayBufferLike = ArrayBuffer>(buffer: TArrayBuffer, byteOffset?: number, length?: number): Float32Array<TArrayBuffer>;
    new(buffer: ArrayBuffer, byteOffset?: number, length?: number): Float32Array<ArrayBuffer>;
    new(array: ArrayLike<number> | ArrayBuffer): Float32Array<ArrayBuffer>;
    new(elements: Iterable<number>): Float32Array<ArrayBuffer>;
}
interface Float64ArrayConstructor extends $TypedArrayConstructor<number, Float64Array<ArrayBufferLike>, Float64Array<ArrayBuffer>> {
    new(length: number): Float64Array<ArrayBuffer>;
    new(array: ArrayLike<number>): Float64Array<ArrayBuffer>;
    new<TArrayBuffer extends ArrayBufferLike = ArrayBuffer>(buffer: TArrayBuffer, byteOffset?: number, length?: number): Float64Array<TArrayBuffer>;
    new(buffer: ArrayBuffer, byteOffset?: number, length?: number): Float64Array<ArrayBuffer>;
    new(array: ArrayLike<number> | ArrayBuffer): Float64Array<ArrayBuffer>;
    new(elements: Iterable<number>): Float64Array<ArrayBuffer>;
}

declare var Int8Array: Int8ArrayConstructor;
declare var Uint8Array: Uint8ArrayConstructor;
declare var Uint8ClampedArray: Uint8ClampedArrayConstructor;
declare var Int16Array: Int16ArrayConstructor;
declare var Uint16Array: Uint16ArrayConstructor;
declare var Int32Array: Int32ArrayConstructor;
declare var Uint32Array: Uint32ArrayConstructor;
declare var Float32Array: Float32ArrayConstructor;
declare var Float64Array: Float64ArrayConstructor;

interface DataView<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> {
    readonly buffer: TArrayBuffer;
    byteLength: number;
    byteOffset: number;
    /**
     * Gets the Int8 value at the specified byte offset from the start of the view. There is
     * no alignment constraint; multi-byte values may be fetched from any offset.
     * @param byteOffset The place in the buffer at which the value should be retrieved.
     */
    getInt8(byteOffset: number): number;
    /**
     * Gets the Uint8 value at the specified byte offset from the start of the view. There is
     * no alignment constraint; multi-byte values may be fetched from any offset.
     * @param byteOffset The place in the buffer at which the value should be retrieved.
     */
    getUint8(byteOffset: number): number;
    /**
     * Gets the Int16 value at the specified byte offset from the start of the view. There is
     * no alignment constraint; multi-byte values may be fetched from any offset.
     * @param byteOffset The place in the buffer at which the value should be retrieved.
     */
    getInt16(byteOffset: number, littleEndian?: boolean): number;
    /**
     * Gets the Uint16 value at the specified byte offset from the start of the view. There is
     * no alignment constraint; multi-byte values may be fetched from any offset.
     * @param byteOffset The place in the buffer at which the value should be retrieved.
     */
    getUint16(byteOffset: number, littleEndian?: boolean): number;
    /**
     * Gets the Int32 value at the specified byte offset from the start of the view. There is
     * no alignment constraint; multi-byte values may be fetched from any offset.
     * @param byteOffset The place in the buffer at which the value should be retrieved.
     */
    getInt32(byteOffset: number, littleEndian?: boolean): number;
    /**
     * Gets the Uint32 value at the specified byte offset from the start of the view. There is
     * no alignment constraint; multi-byte values may be fetched from any offset.
     * @param byteOffset The place in the buffer at which the value should be retrieved.
     */
    getUint32(byteOffset: number, littleEndian?: boolean): number;
    /**
     * Gets the Float32 value at the specified byte offset from the start of the view. There is
     * no alignment constraint; multi-byte values may be fetched from any offset.
     * @param byteOffset The place in the buffer at which the value should be retrieved.
     */
    getFloat32(byteOffset: number, littleEndian?: boolean): number;
    /**
     * Gets the Float64 value at the specified byte offset from the start of the view. There is
     * no alignment constraint; multi-byte values may be fetched from any offset.
     * @param byteOffset The place in the buffer at which the value should be retrieved.
     */
    getFloat64(byteOffset: number, littleEndian?: boolean): number;
    /**
     * Stores an Int8 value at the specified byte offset from the start of the view.
     * @param byteOffset The place in the buffer at which the value should be set.
     * @param value The value to set.
     */
    setInt8(byteOffset: number, value: number): void;
    /**
     * Stores an Uint8 value at the specified byte offset from the start of the view.
     * @param byteOffset The place in the buffer at which the value should be set.
     * @param value The value to set.
     */
    setUint8(byteOffset: number, value: number): void;
    /**
     * Stores an Int16 value at the specified byte offset from the start of the view.
     * @param byteOffset The place in the buffer at which the value should be set.
     * @param value The value to set.
     * @param littleEndian If false or undefined, a big-endian value should be written,
     * otherwise a little-endian value should be written.
     */
    setInt16(byteOffset: number, value: number, littleEndian?: boolean): void;
    /**
     * Stores an Uint16 value at the specified byte offset from the start of the view.
     * @param byteOffset The place in the buffer at which the value should be set.
     * @param value The value to set.
     * @param littleEndian If false or undefined, a big-endian value should be written,
     * otherwise a little-endian value should be written.
     */
    setUint16(byteOffset: number, value: number, littleEndian?: boolean): void;
    /**
     * Stores an Int32 value at the specified byte offset from the start of the view.
     * @param byteOffset The place in the buffer at which the value should be set.
     * @param value The value to set.
     * @param littleEndian If false or undefined, a big-endian value should be written,
     * otherwise a little-endian value should be written.
     */
    setInt32(byteOffset: number, value: number, littleEndian?: boolean): void;
    /**
     * Stores an Uint32 value at the specified byte offset from the start of the view.
     * @param byteOffset The place in the buffer at which the value should be set.
     * @param value The value to set.
     * @param littleEndian If false or undefined, a big-endian value should be written,
     * otherwise a little-endian value should be written.
     */
    setUint32(byteOffset: number, value: number, littleEndian?: boolean): void;
    /**
     * Stores an Float32 value at the specified byte offset from the start of the view.
     * @param byteOffset The place in the buffer at which the value should be set.
     * @param value The value to set.
     * @param littleEndian If false or undefined, a big-endian value should be written,
     * otherwise a little-endian value should be written.
     */
    setFloat32(byteOffset: number, value: number, littleEndian?: boolean): void;
    /**
     * Stores an Float64 value at the specified byte offset from the start of the view.
     * @param byteOffset The place in the buffer at which the value should be set.
     * @param value The value to set.
     * @param littleEndian If false or undefined, a big-endian value should be written,
     * otherwise a little-endian value should be written.
     */
    setFloat64(byteOffset: number, value: number, littleEndian?: boolean): void;
}

interface DataViewConstructor {
    readonly prototype: DataView<ArrayBufferLike>;
    new<TArrayBuffer extends ArrayBufferLike>(buffer: TArrayBuffer, byteOffset?: number, length?: number): DataView<TArrayBuffer>;
}

declare var DataView: DataViewConstructor;

declare function escape(str: string): string;
declare function unescape(str: string): string;

interface ImportMeta {
}

/** Obtain the return type of a function type */
type ReturnType<T extends ((...args: ReadonlyArray<empty>) => unknown) | hook (...args: ReadonlyArray<empty>) => unknown> =
  T extends (...args: ReadonlyArray<empty>) => infer Return ? Return :
  T extends hook (...args: ReadonlyArray<empty>) => infer Return ? Return : any;

/** Obtain the parameters of a function type in a tuple */
type Parameters<T extends ((...args: ReadonlyArray<empty>) => unknown) | hook (...args: ReadonlyArray<empty>) => unknown> =
  T extends (...args: infer Args) => unknown ? Args :
  T extends hook (...args: infer Args) => unknown ? Args : empty;

/** Obtain the parameters of a constructor function type in a tuple */
type ConstructorParameters<T extends new (...args: ReadonlyArray<empty>) => unknown> =
  T extends new (...args: infer Args) => unknown ? Args : empty;

/** Obtain the return (instance) type of a constructor function type */
type InstanceType<T extends new (...args: ReadonlyArray<empty>) => unknown> =
  T extends new (...args: ReadonlyArray<empty>) => infer Return ? Return : any;

/** Exclude from T those types that are assignable to U */
type Exclude<T, U> = T extends U ? empty : T;

/** Extract from T those types that are assignable to U */
type Extract<T, U> = T extends U ? T : empty;

/**
 * Extracts the type of the 'this' parameter of a function type,
 * or 'unknown' if the function type has no 'this' parameter.
 */
type ThisParameterType<T> = T extends (this: infer U, ...args: empty) => unknown ? U : unknown;

/**
 * Removes the 'this' parameter from a function type.
 */
type OmitThisParameter<T> = unknown extends ThisParameterType<T> ? T : T extends (this: infer This, ...args: infer A) => infer R ? (...args: A) => R : T;

type Awaited<T> = T extends Promise<infer U> ? U : T;

/**
 * Extract specific fields from an object, e.g. Pick<O, 'foo' | 'bar'>
 */
type Pick<O extends interface {}, Keys extends keyof O> = {[key in Keys]: O[key]};

/**
 * Omit specific fields from an object, e.g. Omit<O, 'foo' | 'bar'>
 */
type Omit<O extends interface {}, Keys> = $Omit<O, Keys>;

/**
 * Construct an object type using string literals as keys with the given type,
 * e.g. Record<'foo' | 'bar', number> = {foo: number, bar: number}
 */
type Record<K extends PropertyKey, T> = {
    [_key in K]: T;
};

// TS compatibility types

type PropertyKey = string | number | symbol;
type ArrayBufferView<out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> =
  $ArrayBufferView<TArrayBuffer>;
interface RegExpExecArray extends RegExpMatchArray {}
type WeakKey = interface {};

interface IArguments {
  [index: number]: any;
  length: number;
  callee: (...ReadonlyArray<unknown>) => unknown;
}

interface TypedPropertyDescriptor<T> {
  enumerable?: boolean;
  configurable?: boolean;
  writable?: boolean;
  value?: T;
  get?: () => T;
  set?: (value: T) => void;
}

interface PromiseLike<T> {
  then<TResult1 = T, TResult2 = empty>(
    onfulfilled?: ((value: T) => TResult1 | PromiseLike<TResult1>) | null | void,
    onrejected?: ((reason: any) => TResult2 | PromiseLike<TResult2>) | null | void,
  ): PromiseLike<TResult1 | TResult2>;
}

type TemplateStringsArray = ReadonlyArray<string> & { readonly raw: ReadonlyArray<string>, ... };

interface Symbol {
  toString(): string;
  valueOf(): ?symbol;
}

/**
 * Represents the completion of an asynchronous operation
 */
interface Promise<out R = unknown> {
    /**
     * Attaches callbacks for the resolution and/or rejection of the Promise.
     * @param onFulfill The callback to execute when the Promise is resolved.
     * @param onReject The callback to execute when the Promise is rejected.
     * @returns A Promise for the completion of which ever callback is executed.
     */
    then<U = empty>(
      onFulfill: null | void,
      onReject: null | void | ((error: any) => Promise<U> | U)
    ): Promise<R | U>;
    /**
     * Attaches callbacks for the resolution and/or rejection of the Promise.
     * @param onFulfill The callback to execute when the Promise is resolved.
     * @param onReject The callback to execute when the Promise is rejected.
     * @returns A Promise for the completion of which ever callback is executed.
     */
    then<U = unknown>(
      onFulfill: (value: R) => Promise<U> | U,
      onReject: null | void | ((error: any) => Promise<U> | U)
    ): Promise<U>;

    /**
     * Attaches a callback for only the rejection of the Promise.
     * @param onReject The callback to execute when the Promise is rejected.
     * @returns A Promise for the completion of the callback.
     */
    catch<U = empty>(
      onReject: null | void | ((error: any) => Promise<U> | U)
    ): Promise<R | U>;
}
