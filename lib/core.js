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
 * MERCHANTABLITY OR NON-INFRINGEMENT.
 * See the Apache Version 2.0 License for specific language governing permissions
 * and limitations under the License.
 *
 * @flow
 */
// @lint-ignore-every LICENSELINT

declare var NaN: number;
declare var Infinity: number;
declare var undefined: void;

/**
 * Converts a string to an integer.
 * @param string A string to convert into a number.
 * @param radix A value between 2 and 36 that specifies the base of the number in numString.
 * If this argument is not supplied, strings with a prefix of '0x' are considered hexadecimal.
 * All other strings are considered decimal.
 */
declare function parseInt(string: mixed, radix?: number): number;
/**
 * Converts a string to a floating-point number.
 * @param string A string that contains a floating-point number.
 */
declare function parseFloat(string: mixed): number;

/**
 * Returns a boolean value that indicates whether a value is the reserved value NaN (not a number).
 * @param number A numeric value.
 */
declare function isNaN(number: mixed): boolean;
/**
 * Determines whether a supplied number is finite.
 * @param number Any numeric value.
 */
declare function isFinite(number: mixed): boolean;
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
    enumerable?: boolean,
    configurable?: boolean,
    writable?: boolean,
    value?: T,
    get?: () => T,
    set?: (value: T) => void,
    ...
};

type PropertyDescriptorMap = { [s: string]: PropertyDescriptor<any>, ... }

type $NotNullOrVoid =
| number
| string
| boolean
| interface {}
| $ReadOnlyArray<mixed>
| symbol
| bigint
| EnumValue<>
| Enum<>;

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
    static entries(object: interface {}): Array<[string, mixed]>;
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
    static getOwnPropertyDescriptor<T = mixed>(o: $NotNullOrVoid, p: any): PropertyDescriptor<T> | void;
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
    static hasOwn(obj: $NotNullOrVoid, prop: mixed): boolean;
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
    static values(object: interface {}): Array<mixed>;
    /**
     * Determines whether an object has a property with the specified name.
     * @param prop A property name.
     */
    hasOwnProperty(prop: mixed): boolean;
    /**
     * Determines whether an object exists in another object's prototype chain.
     * @param o Another object whose prototype chain is to be checked.
     */
    isPrototypeOf(o: mixed): boolean;
    /**
     * Determines whether a specified property is enumerable.
     * @param prop A property name.
     */
    propertyIsEnumerable(prop: mixed): boolean;
    /** Returns a date converted to a string using the current locale. */
    toLocaleString(): string;
    /** Returns a string representation of an object. */
    toString(): string;
    /** Returns the primitive value of the specified object. */
    valueOf(): mixed;
}

// Well known Symbols.
declare opaque type $SymbolHasInstance: symbol;
declare opaque type $SymboIsConcatSpreadable: symbol;
declare opaque type $SymbolIterator: symbol;
declare opaque type $SymbolMatch: symbol;
declare opaque type $SymbolMatchAll: symbol;
declare opaque type $SymbolReplace: symbol;
declare opaque type $SymbolSearch: symbol;
declare opaque type $SymbolSpecies: symbol;
declare opaque type $SymbolSplit: symbol;
declare opaque type $SymbolToPrimitive: symbol;
declare opaque type $SymbolToStringTag: symbol;
declare opaque type $SymbolUnscopables: symbol;

declare class Symbol {
  /**
   * Returns a new unique Symbol value.
   * @param value Description of the new Symbol object.
   */
  static (value?: mixed): symbol;
  /**
   * A method that returns the default async iterator for an object. Called by the semantics of
   * the for-await-of statement.
   */
  static +asyncIterator: '@@asyncIterator'; // polyfill '@@asyncIterator'
  /**
   * Returns a Symbol object from the global symbol registry matching the given key if found.
   * Otherwise, returns a new symbol with this key.
   * @param key key to search for.
   */
  static for(key: string): symbol;
  /**
   * Expose the [[Description]] internal slot of a symbol directly.
   */
  +description: string | void;
  /**
   * A method that determines if a constructor object recognizes an object as one of the
   * constructor's instances. Called by the semantics of the instanceof operator.
   */
  static +hasInstance: $SymbolHasInstance;
  /**
   * A Boolean value that if true indicates that an object should flatten to its array elements
   * by Array.prototype.concat.
   */
  static +isConcatSpreadable: $SymboIsConcatSpreadable;
  /**
   * A method that returns the default iterator for an object. Called by the semantics of the
   * for-of statement.
   */
  static +iterator: '@@iterator'; // polyfill '@@iterator'
  /**
   * Returns a key from the global symbol registry matching the given Symbol if found.
   * Otherwise, returns a undefined.
   * @param sym Symbol to find the key for.
   */
  static keyFor(sym: symbol): ?string;
  static +length: 0;
  /**
   * A regular expression method that matches the regular expression against a string. Called
   * by the String.prototype.match method.
   */
  static +match: $SymbolMatch;
  /**
   * A regular expression method that matches the regular expression against a string. Called
   * by the String.prototype.matchAll method.
   */
  static +matchAll: $SymbolMatchAll;
  /**
   * A regular expression method that replaces matched substrings of a string. Called by the
   * String.prototype.replace method.
   */
  static +replace: $SymbolReplace;
  /**
   * A regular expression method that returns the index within a string that matches the
   * regular expression. Called by the String.prototype.search method.
   */
  static +search: $SymbolSearch;
  /**
   * A function valued property that is the constructor function that is used to create
   * derived objects.
   */
  static +species: $SymbolSpecies;
  /**
   * A regular expression method that splits a string at the indices that match the regular
   * expression. Called by the String.prototype.split method.
   */
  static +split: $SymbolSplit;
  /**
   * A method that converts an object to a corresponding primitive value.
   * Called by the ToPrimitive abstract operation.
   */
  static +toPrimitive: $SymbolToPrimitive;
  /**
   * A String value that is used in the creation of the default string description of an object.
   * Called by the built-in method Object.prototype.toString.
   */
  static +toStringTag: $SymbolToStringTag;
  /**
   * An Object whose own property names are property names that are excluded from the 'with'
   * environment bindings of the associated objects.
   */
  static +unscopables: $SymbolUnscopables;
  toString(): string;
  valueOf(): ?symbol;
}

// TODO: instance, static
declare class Function {
    proto apply: (<T, R, A: $ArrayLike<mixed> = []>(this: (this: T, ...args: A) => R, thisArg: T, args?: A) => R);
    proto bind: Function$Prototype$Bind; // (thisArg: any, ...argArray: Array<any>) => any;
    proto call: <T, R, A: $ArrayLike<mixed> = []>(this: (this: T, ...args: A) => R, thisArg: T, ...args: A) => R;
    /** Returns a string representation of a function. */
    toString(): string;
    arguments: any;
    caller: any | null;
    +length: number;
    /**
     * Returns the name of the function.
     */
    +name: string;
}

declare class Boolean {
    constructor(value?: mixed): void;
    static (value:mixed):boolean;
    /** Returns the primitive value of the specified object. */
    valueOf(): boolean;
    toString(): string;
}

/** An object that represents a number of any kind. All JavaScript numbers are 64-bit floating-point numbers. */
declare class Number {
    /**
     * The value of Number.EPSILON is the difference between 1 and the smallest value greater than 1
     * that is representable as a Number value, which is approximately:
     * 2.2204460492503130808472633361816 x 10^-16.
     */
    static EPSILON: number;
    /**
     * The value of the largest integer n such that n and n + 1 are both exactly representable as
     * a Number value.
     * The value of Number.MAX_SAFE_INTEGER is 9007199254740991 2^53 - 1.
     */
    static MAX_SAFE_INTEGER: number;
    /** The largest number that can be represented in JavaScript. Equal to approximately 1.79E+308. */
    static MAX_VALUE: number;
    /**
     * The value of the smallest integer n such that n and n - 1 are both exactly representable as
     * a Number value.
     * The value of Number.MIN_SAFE_INTEGER is -9007199254740991 (-(2^53 - 1)).
     */
    static MIN_SAFE_INTEGER: number;
    /** The closest number to zero that can be represented in JavaScript. Equal to approximately 5.00E-324. */
    static MIN_VALUE: number;
    /**
     * A value that is not a number.
     * In equality comparisons, NaN does not equal any value, including itself. To test whether a value is equivalent to NaN, use the isNaN function.
     */
    static NaN: number;
    /**
     * A value that is less than the largest negative number that can be represented in JavaScript.
     * JavaScript displays NEGATIVE_INFINITY values as -infinity.
     */
    static NEGATIVE_INFINITY: number;
    /**
     * A value greater than the largest number that can be represented in JavaScript.
     * JavaScript displays POSITIVE_INFINITY values as infinity.
     */
    static POSITIVE_INFINITY: number;
    static (value:mixed):number;
    /**
     * Returns true if passed value is finite.
     * Unlike the global isFinite, Number.isFinite doesn't forcibly convert the parameter to a
     * number. Only finite values of the type number, result in true.
     * @param value A numeric value.
     */
    static isFinite(value: mixed): implies value is number;
    /**
     * Returns true if the value passed is an integer, false otherwise.
     * @param value A numeric value.
     */
    static isInteger(value: mixed): implies value is number;
    /**
     * Returns a Boolean value that indicates whether a value is the reserved value NaN (not a
     * number). Unlike the global isNaN(), Number.isNaN() doesn't forcefully convert the parameter
     * to a number. Only values of the type number, that are also NaN, result in true.
     * @param value A numeric value.
     */
    static isNaN(value: mixed): implies value is number;
    /**
     * Returns true if the value passed is a safe integer.
     * @param value A numeric value.
     */
    static isSafeInteger(value: mixed): implies value is number;
    /**
     * Converts a string to a floating-point number.
     * @param value A string that contains a floating-point number.
     */
    static parseFloat(value: string): number;
    /**
     * Converts A string to an integer.
     * @param value A string to convert into a number.
     * @param radix A value between 2 and 36 that specifies the base of the number in numString.
     * If this argument is not supplied, strings with a prefix of '0x' are considered hexadecimal.
     * All other strings are considered decimal.
     */
    static parseInt(value: string, radix?: number): number;
    constructor(value?: mixed): void;
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
declare var Math: {
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
     * Returns the inverse hyperbolic cosine of a number.
     * @param x A numeric expression that contains an angle measured in radians.
     */
    acosh(x: number): number,
    /**
     * Returns the arcsine of a number.
     * @param x A numeric expression.
     */
    asin(x: number): number,
    /**
     * Returns the inverse hyperbolic sine of a number.
     * @param x A numeric expression that contains an angle measured in radians.
     */
    asinh(x: number): number,
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
     * Returns the smallest integer greater than or equal to its numeric argument.
     * @param x A numeric expression.
     */
    ceil(x: number): number,
    /**
     * Returns the number of leading zero bits in the 32-bit binary representation of a number.
     * @param x A numeric expression.
     */
    clz32(x: number): number,
    /**
     * Returns the cosine of a number.
     * @param x A numeric expression that contains an angle measured in radians.
     */
    cos(x: number): number,
    /**
     * Returns the hyperbolic cosine of a number.
     * @param x A numeric expression that contains an angle measured in radians.
     */
    cosh(x: number): number,
    /**
     * Returns e (the base of natural logarithms) raised to a power.
     * @param x A numeric expression representing the power of e.
     */
    exp(x: number): number,
    /**
     * Returns the result of (e^x - 1), which is an implementation-dependent approximation to
     * subtracting 1 from the exponential function of x (e raised to the power of x, where e
     * is the base of the natural logarithms).
     * @param x A numeric expression.
     */
    expm1(x: number): number,
    /**
     * Returns the greatest integer less than or equal to its numeric argument.
     * @param x A numeric expression.
     */
    floor(x: number): number,
    /**
     * Returns the nearest 16-bit half precision float representation of a number.
     * @param x A numeric expression.
     */
    f16round(x: number): number,
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
     * Returns the natural logarithm (base e) of a number.
     * @param x A numeric expression.
     */
    log(x: number): number,
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
     * Returns the sign of the x, indicating whether x is positive, negative or zero.
     * @param x The numeric expression to test
     */
    sign(x: number): number,
    /**
     * Returns the sine of a number.
     * @param x A numeric expression that contains an angle measured in radians.
     */
    sin(x: number): number,
    /**
     * Returns the hyperbolic sine of a number.
     * @param x A numeric expression that contains an angle measured in radians.
     */
    sinh(x: number): number,
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
    ...
};

/**
 * A class of Array methods and properties that don't mutate the array.
 */
declare class $ReadOnlyArray<+T> {
    @@iterator(): Iterator<T>;
    /**
     * Returns a string representation of an array. The elements are converted to string using their toLocalString methods.
     */
    toLocaleString(): string;
    /**
     * Returns the item located at the specified index.
     * @param index The zero-based index of the desired item. A negative index will count back from the last item.
     */
    at(index: number): T | void;
    // concat creates a new array
    /**
     * Combines two or more arrays.
     * @param items Additional items to add to the end of array1.
     */
    concat<S = T>(...items: Array<$ReadOnlyArray<S> | S>): Array<T | S>;
    /**
     * Returns an iterable of key, value pairs for every entry in the array
     */
    entries(): Iterator<[number, T]>;
    /**
     * Determines whether all the members of an array satisfy the specified test.
     * @param callbackfn A function that accepts up to three arguments. The every method calls
     * the predicate function for each element in the array until the predicate returns a value
     * which is coercible to the Boolean value false, or until the end of the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    every<This>(callbackfn: (this : This, value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg: This): boolean;
    /**
     * Returns the elements of an array that meet the condition specified in a callback function.
     * @param callbackfn A function that accepts up to three arguments. The filter method calls the predicate function one time for each element in the array.
     */
    filter(callbackfn: typeof Boolean): Array<$NonMaybeType<T>>;
    /**
     * Returns the elements of an array that meet the condition specified in a callback function.
     * @param callbackfn A predicate function that accepts up to three arguments. The filter method calls the predicate function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function. If thisArg is omitted, undefined is used as the this value.
     * @returns An array whose type is specified by the predicate function passed as callbackfn.
     */
    filter<This, S: T>(callbackfn: (this: This, value: T, index: number, array: $ReadOnlyArray<T>) => implies value is S, thisArg: This): Array<S>;
    /**
     * Returns the elements of an array that meet the condition specified in a callback function.
     * @param callbackfn A function that accepts up to three arguments. The filter method calls the predicate function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function. If thisArg is omitted, undefined is used as the this value.
     */
    filter<This>(callbackfn: (this : This, value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg : This): Array<T>;
    /**
     * Returns the value of the first element in the array where predicate is true, and undefined
     * otherwise.
     * @param callbackfn find calls predicate once for each element of the array, in ascending
     * order, until it finds one where predicate returns true. If such an element is found, find
     * immediately returns that element value. Otherwise, find returns undefined.
     * @param thisArg If provided, it will be used as the this value for each invocation of
     * predicate. If it is not provided, undefined is used instead.
     */
    find<This>(callbackfn: (this : This, value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg: This): T | void;
    /**
     * Returns the index of the first element in the array where predicate is true, and -1
     * otherwise.
     * @param callbackfn find calls predicate once for each element of the array, in ascending
     * order, until it finds one where predicate returns true. If such an element is found,
     * findIndex immediately returns that element index. Otherwise, findIndex returns -1.
     * @param thisArg If provided, it will be used as the this value for each invocation of
     * predicate. If it is not provided, undefined is used instead.
     */
    findIndex<This>(callbackfn: (this : This, value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg: This): number;
    /**
     * Returns the value of the last element in the array where predicate is true, and undefined
     * otherwise.
     * @param callbackfn find calls predicate once for each element of the array, in reverse
     * order, until it finds one where predicate returns true. If such an element is found, find
     * immediately returns that element value. Otherwise, find returns undefined.
     * @param thisArg If provided, it will be used as the this value for each invocation of
     * predicate. If it is not provided, undefined is used instead.
     */
    findLast<This>(callbackfn: (this : This, value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg: This): T | void;
    /**
     * Returns the index of the last element in the array where predicate is true, and -1
     * otherwise.
     * @param callbackfn find calls predicate once for each element of the array, in reverse
     * order, until it finds one where predicate returns true. If such an element is found,
     * findLastIndex immediately returns that element index. Otherwise, findLastIndex returns -1.
     * @param thisArg If provided, it will be used as the this value for each invocation of
     * predicate. If it is not provided, undefined is used instead.
     */
    findLastIndex<This>(callbackfn: (this : This, value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg: This): number;
    /**
     * Performs the specified action for each element in an array.
     * @param callbackfn  A function that accepts up to three arguments. forEach calls the callbackfn function one time for each element in the array.
     * @param thisArg  An object to which the this keyword can refer in the callbackfn function. If thisArg is omitted, undefined is used as the this value.
     */
    forEach<This>(callbackfn: (this : This, value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg: This): void;
    /**
     * Determines whether an array includes a certain element, returning true or false as appropriate.
     * @param searchElement The element to search for.
     * @param fromIndex The position in this array at which to begin searching for searchElement.
     */
    includes(searchElement: mixed, fromIndex?: number): implies searchElement is T;
    /**
     * Returns the index of the first occurrence of a value in an array.
     * @param searchElement The value to locate in the array.
     * @param fromIndex The array index at which to begin the search. If fromIndex is omitted, the search starts at index 0.
     */
    indexOf(searchElement: mixed, fromIndex?: number): number;
    /**
     * Adds all the elements of an array separated by the specified separator string.
     * @param separator A string used to separate one element of an array from the next in the resulting String. If omitted, the array elements are separated with a comma.
     */
    join(separator?: string): string;
    /**
     * Returns an iterable of keys in the array
     */
    keys(): Iterator<number>;
    /**
     * Returns the index of the last occurrence of a specified value in an array.
     * @param searchElement The value to locate in the array.
     * @param fromIndex The array index at which to begin the search. If fromIndex is omitted, the search starts at the last index in the array.
     */
    lastIndexOf(searchElement: mixed, fromIndex?: number): number;
    /**
     * Calls a defined callback function on each element of an array, and returns an array that contains the results.
     * @param callbackfn A function that accepts up to three arguments. The map method calls the callbackfn function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the callbackfn function. If thisArg is omitted, undefined is used as the this value.
     */
    map<U, This>(callbackfn: (this : This, value: T, index: number, array: $ReadOnlyArray<T>) => U, thisArg: This): Array<U>;
    /**
     * Calls a defined callback function on each element of an array. Then, flattens the result into
     * a new array.
     * This is identical to a map followed by flat with depth 1.
     *
     * @param callbackfn A function that accepts up to three arguments. The flatMap method calls the
     * callback function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the callback function. If
     * thisArg is omitted, undefined is used as the this value.
     */
    flatMap<U, This>(callbackfn: (this : This, value: T, index: number, array: $ReadOnlyArray<T>) => $ReadOnlyArray<U> | U, thisArg: This): Array<U>;
    /**
     * Returns a new array with all sub-array elements concatenated into it recursively up to the
     * specified depth.
     *
     * @param depth The maximum recursion depth
     */
    flat(depth: 0): Array<T>;
    /**
     * Returns a new array with all sub-array elements concatenated into it recursively up to the
     * specified depth.
     *
     * @param depth The maximum recursion depth
     */
    flat(depth: void | 1): Array<T extends $ReadOnlyArray<infer E> ? E : T>;
    /**
     * Returns a new array with all sub-array elements concatenated into it recursively up to the
     * specified depth.
     *
     * @param depth The maximum recursion depth
     */
    flat(depth: number): Array<mixed>;
    /**
     * Calls the specified callback function for all the elements in an array. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
     * @param callbackfn A function that accepts up to four arguments. The reduce method calls the callbackfn function one time for each element in the array.
     */
    reduce(
      callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: $ReadOnlyArray<T>) => T,
    ): T;
    /**
     * Calls the specified callback function for all the elements in an array. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
     * @param callbackfn A function that accepts up to four arguments. The reduce method calls the callbackfn function one time for each element in the array.
     * @param initialValue If initialValue is specified, it is used as the initial value to start the accumulation. The first call to the callbackfn function provides this value as an argument instead of an array value.
     */
    reduce<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: $ReadOnlyArray<T>) => U,
      initialValue: U
    ): U;
    /**
     * Calls the specified callback function for all the elements in an array, in descending order. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
     * @param callbackfn A function that accepts up to four arguments. The reduceRight method calls the callbackfn function one time for each element in the array.
     */
    reduceRight(
      callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: $ReadOnlyArray<T>) => T,
    ): T;
    /**
     * Calls the specified callback function for all the elements in an array, in descending order. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
     * @param callbackfn A function that accepts up to four arguments. The reduceRight method calls the callbackfn function one time for each element in the array.
     * @param initialValue If initialValue is specified, it is used as the initial value to start the accumulation. The first call to the callbackfn function provides this value as an argument instead of an array value.
     */
    reduceRight<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: $ReadOnlyArray<T>) => U,
      initialValue: U
    ): U;
    /**
     * Returns a section of an array.
     * @param start The beginning of the specified portion of the array.
     * @param end The end of the specified portion of the array. This is exclusive of the element at the index 'end'.
     */
    slice(start?: number, end?: number): Array<T>;
    /**
     * Determines whether the specified callback function returns true for any element of an array.
     * @param callbackfn A function that accepts up to three arguments. The some method calls
     * the predicate function for each element in the array until the predicate returns a value
     * which is coercible to the Boolean value true, or until the end of the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    some<This>(callbackfn: (this : This, value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg: This): boolean;
    /**
     * Returns a new array with the elements in reversed order.
     * It is the copying counterpart of the reverse() method.
     */
    toReversed(): Array<T>;
    /**
     * Returns a new array with the elements sorted in ascending order.
     * It is the copying counterpart of the sort() method.
     * @param compareFn Specifies a function that defines the sort order.
     * If omitted, the array elements are converted to strings, then sorted according
     * to each character's Unicode code point value.
     */
    toSorted(compareFn?: (a: T, b: T) => number): Array<T>;
    /**
     * Returns a new array with some elements removed and/or replaced at a given index.
     * It is the copying counterpart of the splice() method.
     * @param start Zero-based index at which to start changing the array, converted to an integer.
     * @param deleteCount An integer indicating the number of elements in the array to remove from start.
     * @param items The elements to add to the array, beginning from start.
     */
    toSpliced<S>(start: number, deleteCount?: number, ...items: Array<S>): Array<T | S>;
    /**
     * Returns an iterable of values in the array
     */
    values(): Iterator<T>;
    /**
     * Returns a new array with the element at the given index replaced with the given value.
     * It is the copying version of using the bracket notation to change the value of a given index.
     * @param index Zero-based index at which to change the array, converted to an integer.
     * @param value Any value to be assigned to the given index.
     */
    with(index: number, value: T): Array<T>;

    +[key: number]: T;
    /**
     * Gets the length of the array. This is a number one higher than the highest element defined in an array.
     */
    +length: number;
}

declare class Array<T> extends $ReadOnlyArray<T> {
    /**
     * Returns a new JavaScript array with its length property set to that number.
     * (Note: this implies an array of arrayLength empty slots, not slots with actual undefined
     * values. See [sparse arrays](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Indexed_collections#sparse_arrays)).
     */
    constructor(arrayLength?: number): void;
    /**
     * Returns the this object after copying a section of the array identified by start and end
     * to the same array starting at position target
     * @param target If target is negative, it is treated as length+target where length is the
     * length of the array.
     * @param start If start is negative, it is treated as length+start. If end is negative, it
     * is treated as length+end.
     * @param end If not specified, length of the this object is used as its default value.
     */
    copyWithin(target: number, start: number, end?: number): T[];
    /**
     * Determines whether all the members of an array satisfy the specified test.
     * @param callbackfn A function that accepts up to three arguments. The every method calls
     * the predicate function for each element in the array until the predicate returns a value
     * which is coercible to the Boolean value false, or until the end of the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    every<This>(callbackfn: (this : This, value: T, index: number, array: Array<T>) => mixed, thisArg: This): boolean;
    /**
     * Returns the this object after filling the section identified by start and end with value
     * @param value value to fill array section with
     * @param begin index to start filling the array at. If start is negative, it is treated as
     * length+start where length is the length of the array.
     * @param end index to stop filling the array at. If end is negative, it is treated as
     * length+end.
     */
    fill(value: T, begin?: number, end?: number): Array<T>;
    /**
     * Returns the elements of an array that meet the condition specified in a callback function.
     * @param callbackfn A function that accepts up to three arguments. The filter method calls the predicate function one time for each element in the array.
     */
    filter(callbackfn: typeof Boolean): Array<$NonMaybeType<T>>;
    /**
     * Returns the elements of an array that meet the condition specified in a callback function.
     * @param callbackfn A predicate function that accepts up to three arguments. The filter method calls the predicate function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function. If thisArg is omitted, undefined is used as the this value.
     * @returns An array whose type is specified by the predicate function passed as callbackfn.
     */
    filter<This, S: T>(callbackfn: (this: This, value: T, index: number, array: $ReadOnlyArray<T>) => implies value is S, thisArg: This): Array<S>;
    /**
     * Returns the elements of an array that meet the condition specified in a callback function.
     * @param callbackfn A function that accepts up to three arguments. The filter method calls the predicate function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function. If thisArg is omitted, undefined is used as the this value.
     */
    filter<This>(callbackfn: (this : This, value: T, index: number, array: Array<T>) => mixed, thisArg: This): Array<T>;
    /**
     * Returns the value of the first element in the array where predicate is true, and undefined
     * otherwise.
     * @param callbackfn find calls predicate once for each element of the array, in ascending
     * order, until it finds one where predicate returns true. If such an element is found, find
     * immediately returns that element value. Otherwise, find returns undefined.
     * @param thisArg If provided, it will be used as the this value for each invocation of
     * predicate. If it is not provided, undefined is used instead.
     */
    find<This>(callbackfn: (this : This, value: T, index: number, array: Array<T>) => mixed, thisArg: This): T | void;
    /**
     * Returns the index of the first element in the array where predicate is true, and -1
     * otherwise.
     * @param callbackfn  find calls predicate once for each element of the array, in ascending
     * order, until it finds one where predicate returns true. If such an element is found,
     * findIndex immediately returns that element index. Otherwise, findIndex returns -1.
     * @param thisArg If provided, it will be used as the this value for each invocation of
     * predicate. If it is not provided, undefined is used instead.
     */
    findIndex<This>(callbackfn: (this : This, value: T, index: number, array: Array<T>) => mixed, thisArg: This): number;
    /**
     * Performs the specified action for each element in an array.
     * @param callbackfn  A function that accepts up to three arguments. forEach calls the callbackfn function one time for each element in the array.
     * @param thisArg  An object to which the this keyword can refer in the callbackfn function. If thisArg is omitted, undefined is used as the this value.
     */
    forEach<This>(callbackfn: (this : This, value: T, index: number, array: Array<T>) => mixed, thisArg: This): void;
    /**
     * Calls a defined callback function on each element of an array, and returns an array that contains the results.
     * @param callbackfn A function that accepts up to three arguments. The map method calls the callbackfn function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the callbackfn function. If thisArg is omitted, undefined is used as the this value.
     */
    map<U, This>(callbackfn: (this : This, value: T, index: number, array: Array<T>) => U, thisArg: This): Array<U>;
    /**
     * Calls a defined callback function on each element of an array. Then, flattens the result into
     * a new array.
     * This is identical to a map followed by flat with depth 1.
     *
     * @param callbackfn A function that accepts up to three arguments. The flatMap method calls the
     * callback function one time for each element in the array.
     * @param thisArg An object to which the this keyword can refer in the callback function. If
     * thisArg is omitted, undefined is used as the this value.
     */
    flatMap<U, This>(callbackfn: (this : This, value: T, index: number, array: Array<T>) => $ReadOnlyArray<U> | U, thisArg: This): Array<U>;
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
    some<This>(callbackfn: (this : This, value: T, index: number, array: Array<T>) => mixed, thisArg: This): boolean;
    sort(compareFn?: (a: T, b: T) => number): Array<T>;
    splice(start: number, deleteCount?: number, ...items: Array<T>): Array<T>;
    unshift(...items: Array<T>): number;


    [key: number]: T;
    /**
     * Gets or sets the length of the array. This is a number one higher than the highest element defined in an array.
     */
    length: number;
    static (...values:Array<any>): Array<any>;
    static isArray(obj: mixed): boolean;
    /**
     * Creates an array from an iterable object.
     * @param iter An iterable object to convert to an array.
     * @param mapFn A mapping function to call on every element of the array.
     * @param thisArg Value of 'this' used to invoke the mapfn.
     */
    static from<A, B, This>(iter: Iterable<A>, mapFn: (this : This, elem: A, index: number) => B, thisArg: This): Array<B>;
    /**
     * Creates an array from an iterable object.
     * @param iter An iterable object to convert to an array.
     */
    static from<A>(iter: Iterable<A>): Array<A>;
    /**
     * Creates an array from an array-like object.
     * @param arrayLike An array-like object to convert to an array.
     * @param mapFn A mapping function to call on every element of the array.
     * @param thisArg Value of 'this' used to invoke the mapfn.
     */
    static from<A, This>(arrayLike: interface { length: number }, mapFn: (this : This, elem: void, index: number) => A, thisArg: This): Array<A>;
    /**
     * Creates an array from an array-like object.
     * @param arrayLike An array-like object to convert to an array.
     */
    static from(arrayLike: interface { length: number }): Array<void>;
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

type $ArrayLike<T> = interface {
  +[indexer: number]: T;
  @@iterator(): Iterator<T>;
  +length: number;
}

type RegExp$matchResult = Array<string> & {
    index: number,
    input: string,
    groups: ?{ [name: string]: string, ... },
    ...
};

interface TaggedTemplateLiteralArray extends $ReadOnlyArray<string> {
  +raw: $ReadOnlyArray<string>;
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
declare class String {
    @@iterator(): Iterator<string>;
    /**
     * Returns an `<a>` HTML anchor element and sets the name attribute to the text value
     * @param name
     */
    anchor(name: string): string;
    /**
     * Returns a new String consisting of the single UTF-16 code unit located at
     * the specified offset. This method allows for positive and negative integers.
     * Negative integers count back from the last string character.
     * @param index The index (position) of the string character to be returned.
     * Supports relative indexing from the end of the string when passed a negative index;
     * i.e. if a negative number is used, the character returned will be found by
     * counting back from the end of the string.
     * @return A String consisting of the single UTF-16 code unit located at the specified position.
     * Returns undefined if the given index can not be found.
     */
    at(index: number): string | void;
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
     * Returns a nonnegative integer Number less than 1114112 (0x110000) that is the code point
     * value of the UTF-16 encoded code point starting at the string element at position index in
     * the String resulting from converting this object to a String.
     * If there is no element at that position, the result is undefined.
     * If a valid UTF-16 surrogate pair does not begin at index, the result is the code unit at index.
     */
    codePointAt(index: number): number | void;
    /**
     * Returns a string that contains the concatenation of two or more strings.
     * @param strings The strings to append to the end of the string.
     */
    concat(...strings: Array<string>): string;
    constructor(value?: mixed): void;
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
    /** Returns an `<a>` HTML element and sets the href attribute value */
    link(href: string): string;
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
    match(regexp: string | RegExp): RegExp$matchResult | null;
    /**
     * Matches a string with a regular expression, and returns an iterable of matches
     * containing the results of that search.
     * @param regexp A variable name or string literal containing the regular expression pattern and flags.
     */
    matchAll(regexp: string | RegExp): Iterator<RegExp$matchResult>;
    /**
     * Returns the String value result of normalizing the string into the normalization form
     * named by form as specified in Unicode Standard Annex #15, Unicode Normalization Forms.
     * @param format Applicable values: "NFC", "NFD", "NFKC", or "NFKD", If not specified default
     * is "NFC"
     */
    normalize(format?: string): string;
    /**
     * Pads the current string with a given string (possibly repeated) so that the resulting string reaches a given length.
     * The padding is applied from the end (right) of the current string.
     *
     * @param targetLength The length of the resulting string once the current string has been padded.
     *        If this parameter is smaller than the current string's length, the current string will be returned as it is.
     *
     * @param padString The string to pad the current string with.
     *        If this string is too long, it will be truncated and the left-most part will be applied.
     *        The default value for this parameter is " " (U+0020).
     */
    padEnd(targetLength: number, padString?: string): string;
    /**
     * Pads the current string with a given string (possibly repeated) so that the resulting string reaches a given length.
     * The padding is applied from the start (left) of the current string.
     *
     * @param targetLength The length of the resulting string once the current string has been padded.
     *        If this parameter is smaller than the current string's length, the current string will be returned as it is.
     *
     * @param padString The string to pad the current string with.
     *        If this string is too long, it will be truncated and the left-most part will be applied.
     *        The default value for this parameter is " " (U+0020).
     */
    padStart(targetLength: number, padString?: string): string;
    /**
     * Returns a String value that is made from count copies appended together. If count is 0,
     * the empty string is returned.
     * @param count number of copies to append
     */
    repeat(count: number): string;
    /**
     * Replaces text in a string, using a regular expression or search string.
     * @param searchValue A string to search for.
     * @param replaceValue A string containing the text to replace for every successful match of searchValue in this string or a function that returns the replacement text.
     */
    replace(searchValue: string | RegExp, replaceValue: string | (substring: string, ...args: Array<any>) => string): string;
    /**
     * Replace all instances of a substring in a string, using a regular expression or search string.
     * @param searchValue A string or regular expression to search for.
     * @param replaceValue A string containing the text to replace for every successful match of searchValue in this string or a function that returns the replacement text.
     */
    replaceAll(searchValue: string | RegExp, replaceValue: string | (substring: string, ...args: Array<any>) => string): string;
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
     * Returns true if the sequence of elements of searchString converted to a String is the
     * same as the corresponding elements of this object (converted to a String) starting at
     * position. Otherwise returns false.
     */
    startsWith(searchString: string, position?: number): boolean;
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
    /** Removes the trailing white space and line terminator characters from a string. */
    trimEnd(): string;
    /** Removes the leading white space and line terminator characters from a string. */
    trimLeft(): string;
    /** Removes the trailing white space and line terminator characters from a string. */
    trimRight(): string;
    /** Removes the leading white space and line terminator characters from a string. */
    trimStart(): string;
    /** Returns the primitive value of the specified object. */
    valueOf(): string;
    /** Returns a string representation of a string. */
    toString(): string;
    /** Returns the length of a String object. */
    length: number;
    [key: number]: string;
    static (value:mixed):string;
    static fromCharCode(...codes: Array<number>): string;
    /**
     * Return the String value whose elements are, in order, the elements in the List elements.
     * If length is 0, the empty string is returned.
     */
    static fromCodePoint(...codes: Array<number>): string;

    /**
     * String.raw is intended for use as a tag function of a Tagged Template String. When called
     * as such the first argument will be a well formed template call site object and the rest
     * parameter will contain the substitution values.
     * @param callSite A well-formed template string call site representation.
     * @param substitutions A set of substitution values.
     */
    static raw: (callSite: interface {+raw: $ReadOnlyArray<string>}, ...substitutions: $ReadOnlyArray<mixed>) => string;
}

declare class RegExp {
    static (pattern: string | RegExp, flags?: string): RegExp;
    compile(): RegExp;
    constructor(pattern: string | RegExp, flags?: string): void;
    /**
     * Executes a search on a string using a regular expression pattern, and returns an array containing the results of that search.
     * @param string The String object or string literal on which to perform the search.
     */
    exec(string: string): RegExp$matchResult | null;
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
     * Returns a Boolean value indicating the state of the sticky flag (y) used with a regular
     * expression. Default is false. Read-only.
     */
    sticky: boolean;
    /**
     * Returns a Boolean value indicating the state of the Unicode flag (u) used with a regular
     * expression. Default is false. Read-only.
     */
    unicode: boolean;
    /**
     * Returns a Boolean value indicating the state of the dotAll flag (s) used with a regular expression.
     * Default is false. Read-only.
     */
    dotAll: boolean;
    /**
     * Returns a Boolean value that indicates whether or not a pattern exists in a searched string.
     * @param string String on which to perform the search.
     */
    test(string: string): boolean;
    toString(): string;
    +[key: $SymbolMatch | $SymbolMatchAll]: (str: string) => Iterator<RegExp$matchResult>
}

/** Enables basic storage and retrieval of dates and times. */
declare class Date {
    constructor(): void;
    constructor(value: number | Date | string): void;
    constructor(year: number, month: number, day?: number, hour?: number, minute?: number, second?: number, millisecond?: number): void;
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
    toJSON(key?: mixed): string;
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

    static ():string;
    static now(): number;
    /**
     * Parses a string containing a date, and returns the number of milliseconds between that date and midnight, January 1, 1970.
     * @param s A date string
     */
    static parse(s: string): number;
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
    static UTC(year: number, month: number, date?: number, hours?: number, minutes?: number, seconds?: number, ms?: number): number;
    // multiple indexers not yet supported
    [key: $SymbolToPrimitive]: (hint: 'string' | 'default' | 'number') => string | number;
}

declare class CallSite {
    getThis(): any;
    getTypeName(): string;
    getFunction(): ?((...any) => any);
    getFunctionName(): string;
    getMethodName(): string;
    getFileName(): ?string;
    getLineNumber(): ?number;
    getColumnNumber(): ?number;
    getEvalOrigin(): ?CallSite;
    getScriptNameOrSourceURL(): ?string;
    isToplevel(): boolean;
    isEval(): boolean;
    isNative(): boolean;
    isConstructor(): boolean;
    toString(): string;
}

declare class Error {
    static (message?:string, options?: {cause: mixed, ...}):Error;
    constructor (message?: mixed, options?: {cause: mixed, ...}): void;
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

    /** @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/cause */
    cause?: mixed;

    // note: v8 only (node/chrome)
    static captureStackTrace(target: interface { [any] : any }, constructor?: any): void;

    static stackTraceLimit: number;
    static prepareStackTrace: (err: Error, stack: CallSite[]) => mixed;
}

declare class EvalError extends Error {
    static (message?:string):Error;
}

declare class RangeError extends Error {
    static (message?:string):Error;
}

declare class ReferenceError extends Error {
    static (message?:string):Error;
}

declare class SyntaxError extends Error {
    static (message?:string):Error;
}

declare class TypeError extends Error {
    static (message?:string):Error;
}

declare class URIError extends Error {
    static (message?:string):Error;
}

declare class AggregateError extends Error {
  static (errors: Iterable<mixed>, message?: string): Error;
  constructor (errors: Iterable<mixed>, message?: mixed): void;
  errors: Iterable<mixed>;
}

/**
 * An intrinsic object that provides functions to convert JavaScript values to and from the JavaScript Object Notation (JSON) format.
 */
declare var JSON: {|
    /**
     * Converts a JavaScript Object Notation (JSON) string into an object.
     * @param text A valid JSON string.
     * @param reviver A function that transforms the results. This function is called for each member of the object.
     * If a member contains nested objects, the nested objects are transformed before the parent object is.
     */
    +parse: (text: string, reviver?: (key: any, value: any) => any) => any,
    /**
     * Converts a JavaScript value to a JavaScript Object Notation (JSON) string.
     * @param value A JavaScript value, usually an object or array, to be converted.
     * @param replacer A function that transforms the results or an array of strings and numbers that acts as a approved list for selecting the object properties that will be stringified.
     * @param space Adds indentation, white space, and line break characters to the return-value JSON text to make it easier to read.
     */
    +stringify: ((
        value: null | string | number | boolean | interface {} | $ReadOnlyArray<mixed>,
        replacer?: ?((key: string, value: any) => any) | Array<any>,
        space?: string | number
      ) => string) &
      (
        value: mixed,
        replacer?: ?((key: string, value: any) => any) | Array<any>,
        space?: string | number
      ) => string | void,
|};

/* Iterable/Iterator/Generator */

type IteratorResult<+Yield,+Return> =
  | {
    done: true,
    +value?: Return,
    ...
}
  | {
    done: false,
    +value: Yield,
    ...
};

/**
 * The iterator protocol expected by built-ins like Iterator.from().
 * You can implement this yourself.
 */
interface $IteratorProtocol<+Yield,+Return=void,-Next=void> {
    next(value?: Next): IteratorResult<Yield,Return>;
}

/**
 * The built-in Iterator abstract base class. Iterators for Arrays, Strings, and Generators all inherit from this class.
 * Extend this class to implement custom iterators that support all iterator helper methods.
 * Note that you can use `Iterator.from()` to get an iterator helper wrapping any object that implements
 * the iterator protocol.
 */
declare class Iterator<+Yield,+Return=void,-Next=void> implements $IteratorProtocol<Yield,Return,Next>, $Iterable<Yield,Return,Next> {
    @@iterator(): Iterator<Yield,Return,Next>;
    next(value?: Next): IteratorResult<Yield,Return>;
    /**
     * Returns a new iterator that yields that values returned by calling the argument callback on each value yielded by this iterator.
     * @param callbackfn A function that accepts up to two arguments: the yielded value and its index. The map method calls the callbackfn function once per iteration.
     */
    map<U>(callbackfn: (value: Yield, index: number) => U): Iterator<U, void, mixed>;
    /**
     * Returns a new iterator that yields only those elements of the iterator for which the provided predicate returns a truthy value.
     * @param callbackfn A function that accepts up to two arguments: the yielded value and its index. The filter method calls the predicate function once per iteration.
     */
    filter(callbackfn: typeof Boolean): Iterator<$NonMaybeType<Yield>, void, mixed>;
    /**
     * Returns a new iterator that yields only those elements of the iterator for which the provided predicate returns a truthy value.
     * @param callbackfn A function that accepts up to two arguments: the yielded value and its index. The filter method calls the predicate function once per iteration.
     */
    filter<Refined: Yield>(callbackfn: (value: Yield, index: number) => implies value is Refined): Iterator<Refined, void, mixed>;
    /**
     * Returns a new iterator that yields only those elements of the iterator for which the provided predicate returns a truthy value.
     * @param callbackfn A function that accepts up to two arguments: the yielded value and its index. The filter method calls the predicate function once per iteration.
     */
    filter(callbackfn: (value: Yield, index: number) => mixed): Iterator<Yield, void, mixed>;
    /**
     * Returns a new iterator that yields up to the given number of elements in this iterator and then terminates.
     * @param limit The maximum number of values to yield.
     */
    take(limit: number): Iterator<Yield, void, mixed>;
    /**
     * Returns a new iterator that yields values from this iterator after skipping the provided count. If the this iterator has fewer than limit elements, the new iterator will be completed the first time next() is called.
     * @param count The number of values to drop.
     */
    drop(count: number): Iterator<Yield, void, mixed>;
    /**
     * Returns a new iterator that takes each value yielded by this iterator, runs it through the argument mapping function to get another iterable, and yields each value returned by these mapped iterables.
     * @param callbackfn A function that accepts up to two arguments: the yielded value and its index. The map method calls the callbackfn function once per iteration. The callback must return an iterable.
     */
    flatMap<U>(callbackfn: (value: Yield, index: number) => $IteratorProtocol<U, mixed, void> | $Iterable<U, mixed, void>): Iterator<U, void, mixed>;
    /**
     * Calls the argument "reducer" callback function on each value produced by this iterator, passing in the return value from the calculation on the preceding iteration. The final result of running the reducer across all elements is a single value.
     * Exhausts this iterator. Will hang if it's infinite.
     * @param callbackfn A function that accepts up to three arguments: the accumulated value, the current value, and the current index.
     * @param initialValue If initialValue is specified, it is used as the initial value to start
     * the accumulation. If omitted, the first call of the callback gets the first two iterated values.
     */
    reduce<U>(
      callbackfn: (previousValue: Yield | U, currentValue: Yield, index: number) => U,
    ): Yield | U;
    /**
     * Calls the argument "reducer" callback function on each value produced by this iterator, passing in the return value from the calculation on the preceding iteration. The final result of running the reducer across all elements is a single value.
     * Exhausts this iterator. Will hang if it's infinite.
     * @param callbackfn A function that accepts up to three arguments: the accumulated value, the current value, and the current index.
     * @param initialValue If initialValue is specified, it is used as the initial value to start
     * the accumulation. If omitted, the first call of the callback gets the first two iterated values.
     */
    reduce<U>(
      callbackfn: (previousValue: U, currentValue: Yield, index: number) => U,
      initialValue: U
    ): U;
    /**
     * Creates a new array from the values yielded by this iterator.
     * Exhausts this iterator. Will hang if it's infinite.
     */
    toArray(): Array<Yield>;
    /**
     * Calls the argument function for each value yielded by the iterator. Any returned values are ignored.
     * Exhausts this iterator. Will hang if it's infinite.
     * @param callbackfn  A function that accepts up to two arguments: the yielded value and its index.
     */
    forEach(callbackfn: (value: Yield, index: number) => mixed): void;
    /**
     * Calls the argument function for each value yielded by the iterator.
     * Returns true if the predicate function ever returns a truthy value.
     * Exhausts this iterator. Will hang if it's infinite.
     * @param callbackfn  A function that accepts up to two arguments: the yielded value and its index.
     */
    some(callbackfn: (value: Yield, index: number) => mixed): boolean;
    /**
     * Calls the argument function for each value yielded by the iterator.
     * Returns true if the predicate function returns a truthy value for all iterated values.
     * Exhausts this iterator. Will hang if it's infinite.
     * @param callbackfn  A function that accepts up to two arguments: the yielded value and its index.
     */
    every(callbackfn: (value: Yield, index: number) => mixed): boolean;
    /**
     * Calls the argument function for values yielded by the iterator until one returns a truthy value.
     * Returns the first value that produced a truthy predicate.
     * Returns undefined if the iterator completes without a match.
     * Iterates this iterator. Will hang if it's infinite and has no matches.
     * @param callbackfn  A function that accepts up to two arguments: the yielded value and its index.
     */
    find<Refined: Yield>(predicate: (value: Yield, index: number) => implies value is Refined): Refined | void;
    find(callbackfn: (value: Yield, index: number) => mixed): Yield | void;
    /**
     * This method allows wrapping objects that implement the iterator protocol in an Iterator to get the built-in helpers.
     * @returns The argument object if it's already an Iterator subclass, or a wrapping Iterator if the argument objects implements the iterable or iterator protocols.
     */
    static from<+Yield,+Return,-Next>(source: $IteratorProtocol<Yield,Return,Next> | $Iterable<Yield,Return,Next>): Iterator<Yield,Return,Next>;
}

type $Iterator<+Yield,+Return,-Next> = Iterator<Yield,Return,Next>;

/**
 * The iterable protocol expected by built-ins like Array.from().
 * You can implement this yourself.
 */
interface $Iterable<+Yield,+Return,-Next> {
    @@iterator(): $IteratorProtocol<Yield,Return,Next>;
}
type Iterable<+T> = $Iterable<T,void,void>;

// The generator class is not exposed in the global namespace.
declare class $Generator<+Yield,+Return,-Next> extends Iterator<Yield,Return,Next> {
    return<R>(value: R): IteratorResult<Yield,R|Return>;
    throw(error?: any): IteratorResult<Yield,Return>;
}

/**
 * Built-in Generator objects returned by generator functions.
 */
type Generator<+Yield,+Return,-Next> = $Generator<Yield,Return,Next>;

declare function $iterate<T>(p: Iterable<T>): T;

/* Async Iterable/Iterator/Generator */

interface $AsyncIterator<+Yield,+Return,-Next> {
    @@asyncIterator(): $AsyncIterator<Yield,Return,Next>;
    next(value?: Next): Promise<IteratorResult<Yield,Return>>;
}
type AsyncIterator<+T> = $AsyncIterator<T,void,void>;

interface $AsyncIterable<+Yield,+Return,-Next> {
    @@asyncIterator(): $AsyncIterator<Yield,Return,Next>;
}
type AsyncIterable<+T> = $AsyncIterable<T,void,void>;

interface AsyncGenerator<+Yield,+Return,-Next> {
    @@asyncIterator(): $AsyncIterator<Yield,Return,Next>;
    next(value?: Next): Promise<IteratorResult<Yield,Return>>;
    return<R>(value: R): Promise<IteratorResult<Yield,R|Return>>;
    throw(error?: any): Promise<IteratorResult<Yield,Return>>;
}

/* Type used internally for infering the type of the yield delegate */
type $IterableOrAsyncIterableInternal<Input, +Yield, +Return, -Next> =
  Input extends $AsyncIterable<any, any, any>
    ? $AsyncIterable<Yield, Return, Next>
    : $Iterable<Yield, Return, Next>;

/* Maps and Sets */

declare class $ReadOnlyMap<K, +V> {
    @@iterator(): Iterator<[K, V]>;
    /**
     * Returns an iterable of key, value pairs for every entry in the map.
     */
    entries(): Iterator<[K, V]>;
    forEach<This>(callbackfn: (this : This, value: V, index: K, map: $ReadOnlyMap<K, V>) => mixed, thisArg: This): void;
    get(key: K): V | void;
    has(key: K): boolean;
    /**
     * Returns an iterable of keys in the map
     */
    keys(): Iterator<K>;
    size: number;
    /**
     * Returns an iterable of values in the map
     */
    values(): Iterator<V>;
}

declare class Map<K, V> extends $ReadOnlyMap<K, V> {
    @@iterator(): Iterator<[K, V]>;
    constructor(iterable?: ?Iterable<[K, V]>): void;
    clear(): void;
    delete(key: K): boolean;
    /**
     * Returns an iterable of key, value pairs for every entry in the map.
     */
    entries(): Iterator<[K, V]>;
    forEach<This>(callbackfn: (this : This, value: V, index: K, map: Map<K, V>) => mixed, thisArg: This): void;
    get(key: K): V | void;
    has(key: K): boolean;
    /**
     * Returns an iterable of keys in the map
     */
    keys(): Iterator<K>;
    set(key: K, value: V): Map<K, V>;
    size: number;
    /**
     * Returns an iterable of values in the map
     */
    values(): Iterator<V>;
    // Multiple Indexers not yet supported
    +[key: $SymbolToStringTag]: any;
    static +[key: $SymbolSpecies]: any;
}

type WeaklyReferenceable = interface {} | $ReadOnlyArray<mixed>;

/**
 * A WeakRef object lets you hold a weak reference to another object,
 * without preventing that object from getting garbage-collected.
 *
 * @see {@link https://caniuse.com/?search=weakref} See browser support
 */
declare class WeakRef<T: WeaklyReferenceable> {
  constructor(targetObject: T): void;
  /**
  * @return the WeakRef object's target object,
  * or undefined if the target object has been reclaimed.
  */
  deref(): T | void;
}

/**
 * This class provides a way to request that a cleanup callback be called after
 * a registered target has been garbage-collected.
 */
declare class FinalizationRegistry<
  THeldValue,
  TTarget: WeaklyReferenceable = WeaklyReferenceable,
  TUnregisterToken: WeaklyReferenceable = WeaklyReferenceable,
> {
  /**
   * @param cleanupCallbackFn The function that will be called after a
   * registered target has been garbage-collected.
   */
  constructor(cleanupCallbackFn: (THeldValue) => void): void;

  /**
   * Start listening for the garbage collection of the given target.
   *
   * @param target The target to listen to.
   *
   * @param heldValue The value that will be passed to the cleanupCallbackFn
   * when the target is garbage collected.
   *
   * @param unregisterToken A optional token that, if provided, can be passed to
   * the unregister() method to stop listening to the given target.
   */
  register(
    target: TTarget,
    heldValue: THeldValue,
    unregisterToken?: TUnregisterToken,
  ): void;

  /**
   * Stop listening for the garbage collection of all targets that were
   * registered with the given unregisterToken.
   *
   * @return true if at least one target was unregistered and false oterhwise.
   */
  unregister(unregisterToken: TUnregisterToken): boolean;
}

declare class $ReadOnlyWeakMap<K: WeaklyReferenceable, +V> {
    get(key: K): V | void;
    has(key: K): boolean;
}

declare class WeakMap<K: WeaklyReferenceable, V> extends $ReadOnlyWeakMap<K, V> {
    constructor(iterable?: ?Iterable<[K, V]>): void;
    delete(key: K): boolean;
    get(key: K): V | void;
    has(key: K): boolean;
    set(key: K, value: V): WeakMap<K, V>;
}

declare class $ReadOnlySet<+T> {
    @@iterator(): Iterator<T>;
    /**
     * Returns an iterable of [v,v] pairs for every value `v` in the set.
     */
    entries(): Iterator<[T, T]>;
    forEach<This>(callbackfn: (this: This, value: T, index: T, set: $ReadOnlySet<T>) => mixed, thisArg: This): void;
    has(value: T): boolean;
    /**
     * Despite its name, returns an iterable of the values in the set,
     */
    keys(): Iterator<T>;
    size: number;
    /**
     * Returns an iterable of values in the set.
     */
    values(): Iterator<T>;
    /** Takes a `Set` and returns a new `Set` containing elements in this `Set` but not in the given `Set`. */
    difference(other: $ReadOnlySet<T>): Set<T>;
    /** Takes a `Set` and returns a new `Set` containing elements in both this `Set` and the given `Set`. */
    intersection(other: $ReadOnlySet<T>): Set<T>;
    /** Takes a `Set` and returns a `boolean` indicating if this `Set` has no elements in common with the given `Set`. */
    isDisjointFrom(other: $ReadOnlySet<T>): boolean;
    /** Takes a `Set` and returns a `boolean` indicating if all elements of this `Set` are in the given `Set`. */
    isSubsetOf(other: $ReadOnlySet<T>): boolean;
    /** Takes a `Set` and returns a `boolean` indicating if all elements of the given `Set` are in this `Set`. */
    isSupersetOf(other: $ReadOnlySet<T>): boolean;
    /** Returns a new `Set` containing elements which are in either this `Set` or the given `Set`, but not in both. */
    symmetricDifference(other: $ReadOnlySet<T>): Set<T>;
}

declare class Set<T> extends $ReadOnlySet<T> {
    @@iterator(): Iterator<T>;
    constructor(iterable?: ?Iterable<T>): void;
    add(value: T): Set<T>;
    clear(): void;
    delete(value: T): boolean;
    /**
     * Returns an iterable of [v,v] pairs for every value `v` in the set.
     */
    entries(): Iterator<[T, T]>;
    forEach<This>(callbackfn: (this: This, value: T, index: T, set: Set<T>) => mixed, thisArg: This): void;
    has(value: T): boolean;
    /**
     * Despite its name, returns an iterable of the values in the set,
     */
    keys(): Iterator<T>;
    size: number;
    /**
     * Returns an iterable of values in the set.
     */
    values(): Iterator<T>;
    +[key: $SymbolToStringTag]: (...any) => any;
    static +[key: $SymbolSpecies]: (...any) => any; // This would the Set constructor, can't think of a way to correctly type this
}

declare class $ReadOnlyWeakSet<T: WeaklyReferenceable> {
    has(value: T): boolean;
}

declare class WeakSet<T: WeaklyReferenceable> extends $ReadOnlyWeakSet<T> {
    constructor(iterable?: ?Iterable<T>): void;
    add(value: T): WeakSet<T>;
    delete(value: T): boolean;
    has(value: T): boolean;
}

/* Promises
   cf. https://github.com/borisyankov/DefinitelyTyped/blob/master/es6-promises/es6-promises.d.ts
*/
/**
 * Represents the completion of an asynchronous operation
 */
declare class Promise<+R = mixed> {
    constructor(callback: (
      resolve: (result: Promise<R> | R) => void,
      reject: (error: any) => void
    ) => mixed): void;

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
    then<U = mixed>(
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

    /**
     * Attaches a callback that is invoked when the Promise is settled (fulfilled or rejected). The
     * resolved value cannot be modified from the callback.
     * @param onFinally The callback to execute when the Promise is settled (fulfilled or rejected).
     * @returns A Promise for the completion of the callback.
     */
    finally(onFinally: () => mixed): Promise<R>;

    /**
     * Creates a new resolved promise for the provided value.
     * @param object A promise.
     * @returns A promise whose internal state matches the provided promise.
     */
    static resolve<T = mixed>(object: Promise<T> | T): Promise<T>;
    /**
     * Creates a new rejected promise for the provided reason.
     * @param error The reason the promise was rejected.
     * @returns A new rejected Promise.
     */
    static reject<T = mixed>(error: any): Promise<T>;
    /**
     * Creates a Promise that is resolved with an array of results when all of the provided Promises
     * resolve, or rejected when any Promise is rejected.
     * @param promises An iterable of Promises.
     * @returns A new Promise.
     */
    static all<T: Iterable<mixed>>(promises: T): Promise<
      T extends $ReadOnlyArray<mixed> ? {[K in keyof T]: Awaited<T[K]>} :
      T extends Iterable<infer V> ? Array<Awaited<V>> : any
    >;
    /**
     * Creates a Promise that is resolved with an array of results when all
     * of the provided Promises resolve or reject.
     * @param promises  An array of Promises.
     * @returns A new Promise.
     */
    static allSettled<T: Iterable<mixed>>(promises: T): Promise<
      T extends $ReadOnlyArray<mixed> ? {[K in keyof T]: $SettledPromiseResult<Awaited<T[K]>>} :
      T extends Iterable<infer V> ? Array<$SettledPromiseResult<Awaited<V>>> : any
    >;
    /**
     * Creates a Promise that is resolved or rejected when any of the provided Promises are resolved
     * or rejected.
     * @param promises An iterable of Promises.
     * @returns A new Promise.
     */
    static race<T, Elem: Promise<T> | T>(promises: Iterable<Elem>): Promise<T>;
    /**
     * Creates a Promise that fulfills as soon as any of the promises in the iterable fulfills,
     * with the value of the fulfilled promise. If no promises in the iterable fulfill then the
     * returned promise is rejected with an AggregateError.
     * @param promises An iterable of Promises.
     * @returns A new Promise.
     */
    static any<T, Elem: Promise<T> | T>(promises: Iterable<Elem>): Promise<T>;
}

type $SettledPromiseResult<+T> = {|
  +status: 'fulfilled',
  +value: T,
|} | {|
  +status: 'rejected',
  +reason: any,
|};

/* Binary data */

declare class ArrayBuffer {
    static isView(arg: mixed): boolean;
    constructor(byteLength: number): void;
    byteLength: number;
    slice(begin: number, end?: number): this;
    static +[key: $SymbolSpecies]: Class<this>;
}

// This is a helper type to simplify the specification, it isn't an interface
// and there are no objects implementing it.
// https://developer.mozilla.org/en-US/docs/Web/API/ArrayBufferView
type $ArrayBufferView = $TypedArray | DataView;

type $TypedArrayNumber = $TypedArrayInternal<number>;

type $TypedArray = $TypedArrayNumber | BigInt64Array | BigUint64Array;

// The TypedArray intrinsic object is a constructor function, but does not have
// a global name or appear as a property of the global object.
// http://www.ecma-international.org/ecma-262/6.0/#sec-%typedarray%-intrinsic-object
declare class $TypedArrayInternal<T: number | bigint> {
    /**
     * The size in bytes of each element in the array.
     */
    static BYTES_PER_ELEMENT: number;
    static from<This>(iterable: Iterable<T>, mapFn?: (this : This, element: T) => T, thisArg: This): this;
    static of(...values: T[]): this;

    constructor(length: number): void;
    constructor(typedArray: $TypedArray | Iterable<T> | ArrayBuffer): void;
    constructor(buffer: ArrayBuffer, byteOffset?: number, length?: number): void;

    [index: number]: T;

    @@iterator(): Iterator<T>;

    /**
     * The ArrayBuffer instance referenced by the array.
     */
    buffer: ArrayBuffer;
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
     * Returns the item located at the specified index.
     * @param index The zero-based index of the desired item. A negative index will count back from the last item.
     */
    at(index: number): T | void;
    /**
     * Returns the this object after copying a section of the array identified by start and end
     * to the same array starting at position target
     * @param target If target is negative, it is treated as length+target where length is the
     * length of the array.
     * @param start If start is negative, it is treated as length+start. If end is negative, it
     * is treated as length+end.
     * @param end If not specified, length of the this object is used as its default value.
     */
    copyWithin(target: number, start: number, end?: number): void;
    /**
     * Returns an array of key, value pairs for every entry in the array
     */
    entries(): Iterator<[number, T]>;
    /**
     * Determines whether all the members of an array satisfy the specified test.
     * @param callback A function that accepts up to three arguments. The every method calls
     * the predicate function for each element in the array until the predicate returns a value
     * which is coercible to the Boolean value false, or until the end of the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    every<This>(callback: (this : This, value: T, index: number, array: this) => mixed, thisArg: This): boolean;
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
    filter<This>(callback: (this : This, value: T, index: number, array: this) => mixed, thisArg: This): this;
    /**
     * Returns the value of the first element in the array where predicate is true, and undefined
     * otherwise.
     * @param callback find calls predicate once for each element of the array, in ascending
     * order, until it finds one where predicate returns true. If such an element is found, find
     * immediately returns that element value. Otherwise, find returns undefined.
     * @param thisArg If provided, it will be used as the this value for each invocation of
     * predicate. If it is not provided, undefined is used instead.
     */
    find<This> (callback: (this : This, value: T, index: number, array: this) => mixed, thisArg: This): T | void;
    /**
     * Returns the index of the first element in the array where predicate is true, and -1
     * otherwise.
     * @param callback find calls predicate once for each element of the array, in ascending
     * order, until it finds one where predicate returns true. If such an element is found,
     * findIndex immediately returns that element index. Otherwise, findIndex returns -1.
     * @param thisArg If provided, it will be used as the this value for each invocation of
     * predicate. If it is not provided, undefined is used instead.
     */
    findIndex<This>(callback: (this : This, value: T, index: number, array: this) => mixed, thisArg: This): number;
    /**
     * Performs the specified action for each element in an array.
     * @param callback  A function that accepts up to three arguments. forEach calls the
     * callbackfn function one time for each element in the array.
     * @param thisArg  An object to which the this keyword can refer in the callbackfn function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    forEach<This>(callback: (this : This, value: T, index: number, array: this) => mixed, thisArg: This): void;
    /**
     * Determines whether an array includes a certain element, returning true or false as appropriate.
     * @param searchElement The element to search for.
     * @param fromIndex The position in this array at which to begin searching for searchElement.
     */
    includes(searchElement: T, fromIndex?: number): boolean;
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
     * Returns an list of keys in the array
     */
    keys(): Iterator<number>;
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
    map<This>(callback: (this : This, currentValue: T, index: number, array: this) => number, thisArg : This): this;
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
      callback: (previousValue: T, currentValue: T, index: number, array: this) => number,
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
    set(array: Array<T> | $TypedArray, offset?: number): void;
    /**
     * Returns a section of an array.
     * @param start The beginning of the specified portion of the array.
     * @param end The end of the specified portion of the array. This is exclusive of the element at the index 'end'.
     */
    slice(begin?: number, end?: number): this;
    /**
     * Determines whether the specified callback function returns true for any element of an array.
     * @param callback A function that accepts up to three arguments. The some method calls
     * the predicate function for each element in the array until the predicate returns a value
     * which is coercible to the Boolean value true, or until the end of the array.
     * @param thisArg An object to which the this keyword can refer in the predicate function.
     * If thisArg is omitted, undefined is used as the this value.
     */
    some<This>(callback: (this : This, value: T, index: number, array: this) => mixed, thisArg: This): boolean;
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
    /**
     * Returns an list of values in the array
     */
    values(): Iterator<T>;
}

/**
 * A typed array of 8-bit integer values. The contents are initialized to 0. If the requested
 * number of bytes could not be allocated an exception is raised.
 */
declare class Int8Array extends $TypedArrayInternal<number> {}
/**
 * A typed array of 8-bit unsigned integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
declare class Uint8Array extends $TypedArrayInternal<number> {}
/**
 * A typed array of 8-bit unsigned integer (clamped) values. The contents are initialized to 0.
 * If the requested number of bytes could not be allocated an exception is raised.
 */
declare class Uint8ClampedArray extends $TypedArrayInternal<number> {}
/**
 * A typed array of 16-bit signed integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
declare class Int16Array extends $TypedArrayInternal<number> {}
/**
 * A typed array of 16-bit unsigned integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
declare class Uint16Array extends $TypedArrayInternal<number> {}
/**
 * A typed array of 32-bit signed integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
declare class Int32Array extends $TypedArrayInternal<number> {}
/**
 * A typed array of 32-bit unsigned integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
declare class Uint32Array extends $TypedArrayInternal<number> {}
/**
 * A typed array of 16-bit float values. The contents are initialized to 0. If the requested number
 * of bytes could not be allocated an exception is raised.
 */
declare class Float16Array extends $TypedArrayInternal<number> {}
/**
 * A typed array of 32-bit float values. The contents are initialized to 0. If the requested number
 * of bytes could not be allocated an exception is raised.
 */
declare class Float32Array extends $TypedArrayInternal<number> {}
/**
 * A typed array of 64-bit float values. The contents are initialized to 0. If the requested
 * number of bytes could not be allocated an exception is raised.
 */
declare class Float64Array extends $TypedArrayInternal<number> {}

/**
 * A typed array of 64-bit signed integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
declare class BigInt64Array extends $TypedArrayInternal<bigint> {}

/**
 * A typed array of 64-bit unsigned integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
declare class BigUint64Array extends $TypedArrayInternal<bigint> {}

declare class DataView {
    constructor(buffer: ArrayBuffer, byteOffset?: number, length?: number): void;
    buffer: ArrayBuffer;
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
     * Gets the Float16 value at the specified byte offset from the start of the view. There is
     * no alignment constraint; multi-byte values may be fetched from any offset.
     * @param byteOffset The place in the buffer at which the value should be retrieved.
     */
    getFloat16(byteOffset: number, littleEndian?: boolean): number;
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
     * Stores an Float16 value at the specified byte offset from the start of the view.
     * @param byteOffset The place in the buffer at which the value should be set.
     * @param value The value to set.
     * @param littleEndian If false or undefined, a big-endian value should be written,
     * otherwise a little-endian value should be written.
     */
    setFloat16(byteOffset: number, value: number, littleEndian?: boolean): void;
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

declare function btoa(rawString: string): string;
declare function atob(encodedString: string): string;

declare function escape(str: string): string;
declare function unescape(str: string): string;

declare opaque type TimeoutID;
declare opaque type IntervalID;
declare function clearInterval(intervalId: ?IntervalID): void;
declare function clearTimeout(timeoutId: ?TimeoutID): void;
declare function setTimeout<TArguments: Array<mixed>>(
  callback: (...args: TArguments) => mixed,
  ms?: number,
  ...args: TArguments
): TimeoutID;
declare function setInterval<TArguments: Array<mixed>>(
  callback: (...args: TArguments) => mixed,
  ms?: number,
  ...args: TArguments
): IntervalID;
declare function queueMicrotask<TArguments: $ReadOnlyArray<mixed>>(
  callback: (...args: TArguments) => mixed,
): void;

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

/* CommonJS */

declare var global: any;

declare var module: {
    exports: any,
    require(id: string): any,
    id: string,
    filename: string,
    loaded: boolean,
    parent: any,
    children: Array<any>,
    path: string,
    paths: Array<string>,
    isPreloading: boolean,
    ...
};
declare var require: {
    (id: string): any,
    resolve: (id: string, options?: { paths?: Array<string>, ... }) => string,
    cache: any,
    main: typeof module,
    ...
};
declare var exports: {-[key: string]: mixed};

/* Opaque type for module reference magic strings */
declare opaque type $Flow$ModuleRef<+T>;
declare opaque type $Flow$EsmModuleMarkerWrapperInModuleRef<+T>: T;
/* Commonly available, shared between node and dom */
declare var console: {
    assert(condition: mixed, ...data: Array<any>): void,
    clear(): void,
    count(label?: string): void,
    countReset(label?: string): void,
    debug(...data: Array<any>): void,
    dir(...data: Array<any>): void,
    dirxml(...data: Array<any>): void,
    error(...data: Array<any>): void,
    _exception(...data: Array<any>): void,
    group(...data: Array<any>): void,
    groupCollapsed(...data: Array<any>): void,
    groupEnd(): void,
    info(...data: Array<any>): void,
    log(...data: Array<any>): void,
    profile(name?: string): void,
    profileEnd(name?: string): void,
    table(tabularData: { [key: string]: any, ... } | Array<{ [key: string]: any, ... }> | Array<Array<any>>): void,
    time(label?: string): void,
    timeEnd(label: string): void,
    timeStamp(label?: string): void,
    timeLog(label?: string, ...data?: Array<any>): void,
    trace(...data: Array<any>): void,
    warn(...data: Array<any>): void,
    ...
};

type $EnumProto<TEnum, TEnumValue, TRepresentationType> = {|
  cast(this: TEnum, input: ?TRepresentationType): void | TEnumValue,
  getName(this: TEnum, input: TEnumValue): string,
  isValid(this: TEnum, input: ?TRepresentationType | TEnumValue): boolean,
  members(this: TEnum): Iterator<TEnumValue>,
  __proto__: null,
|}

declare class SharedArrayBuffer {
  constructor(byteLength: number): void;

  /**
   * Read-only. The length of the ArrayBuffer (in bytes).
   */
  +byteLength: number;
  /**
   * Returns a section of an SharedArrayBuffer.
   */
  slice(begin?: number, end?: number): this;

  +[key: $SymbolToStringTag]: 'SharedArrayBuffer';
}

type $SharedIntegerTypedArray =
  | Int8Array
  | Uint8Array
  | Int16Array
  | Uint16Array
  | Int32Array
  | Uint32Array

declare var Atomics: {
  /**
   * Adds a value to the value at the given position in the array, returning the original value.
   * Until this atomic operation completes, any other read or write operation against the array
   * will block.
   */
  add(typedArray: $SharedIntegerTypedArray, index: number, value: number): number,
  /**
   * Stores the bitwise AND of a value with the value at the given position in the array,
   * returning the original value. Until this atomic operation completes, any other read or
   * write operation against the array will block.
   */
  and(typedArray: $SharedIntegerTypedArray, index: number, value: number): number,
  /**
   * Replaces the value at the given position in the array if the original value equals the given
   * expected value, returning the original value. Until this atomic operation completes, any
   * other read or write operation against the array will block.
   */
  compareExchange(typedArray: $SharedIntegerTypedArray, index: number, expectedValue: number, replacementValue: number): number,
  /**
   * Replaces the value at the given position in the array, returning the original value. Until
   * this atomic operation completes, any other read or write operation against the array will
   * block.
   */
  exchange(typedArray: $SharedIntegerTypedArray, index: number, value: number): number,
  /**
   * Returns the value at the given position in the array. Until this atomic operation completes,
   * any other read or write operation against the array will block.
   */
  load(typedArray: $SharedIntegerTypedArray, index: number): number,
  /**
   * Stores the bitwise OR of a value with the value at the given position in the array,
   * returning the original value. Until this atomic operation completes, any other read or write
   * operation against the array will block.
   */
  or(typedArray: $SharedIntegerTypedArray, index: number, value: number): number,
  /**
   * Stores a value at the given position in the array, returning the new value. Until this
   * atomic operation completes, any other read or write operation against the array will block.
   */
  store(typedArray: $SharedIntegerTypedArray, index: number, value: number): number,
  /**
   * Subtracts a value from the value at the given position in the array, returning the original
   * value. Until this atomic operation completes, any other read or write operation against the
   * array will block.
   */
  sub(typedArray: $SharedIntegerTypedArray, index: number, value: number): number,
  /**
   * Stores the bitwise XOR of a value with the value at the given position in the array,
   * returning the original value. Until this atomic operation completes, any other read or write
   * operation against the array will block.
   */
  xor(typedArray: $SharedIntegerTypedArray, index: number, value: number): number,

  /**
   * Returns a value indicating whether high-performance algorithms can use atomic operations
   * (`true`) or must use locks (`false`) for the given number of bytes-per-element of a typed
   * array.
   */
  isLockFree(size: number): boolean,
  /**
   * If the value at the given position in the array is equal to the provided value, the current
   * agent is put to sleep causing execution to suspend until the timeout expires (returning
   * `"timed-out"`) or until the agent is awoken (returning `"ok"`); otherwise, returns
   * `"not-equal"`.
   */
  wait(typedArray: Int32Array, index: number, value: number, timeout?: number): 'ok' | 'not-equal' | 'timed-out',
  /**
   * Wakes up sleeping agents that are waiting on the given index of the array, returning the
   * number of agents that were awoken.
   * @param typedArray A shared Int32Array.
   * @param index The position in the typedArray to wake up on.
   * @param count The number of sleeping agents to notify. Defaults to +Infinity.
   */
  notify(typedArray: Int32Array, index: number, count: number): number,

  +[key: $SymbolToStringTag]: 'Atomics',
  ...
};

type Import$Meta = {
  [string]: mixed,
  url?: string,
  ...
};

declare class BigInt {
  static (value: boolean | string | number | bigint | interface {} | $ReadOnlyArray<mixed>): bigint;
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

/** Obtain the return type of a function type */
type ReturnType<T: ((...args: $ReadOnlyArray<empty>) => mixed) | hook (...args: $ReadOnlyArray<empty>) => mixed> =
  T extends (...args: $ReadOnlyArray<empty>) => infer Return ? Return :
  T extends hook (...args: $ReadOnlyArray<empty>) => infer Return ? Return : any;

/** Obtain the parameters of a function type in a tuple */
type Parameters<T: ((...args: $ReadOnlyArray<empty>) => mixed) | hook (...args: $ReadOnlyArray<empty>) => mixed> =
  T extends (...args: infer Args) => mixed ? Args :
  T extends hook (...args: infer Args) => mixed ? Args : empty;

/** Exclude from T those types that are assignable to U */
type Exclude<T, U> = T extends U ? empty : T;

/** Extract from T those types that are assignable to U */
type Extract<T, U> = T extends U ? T : empty;

/**
 * Extracts the type of the 'this' parameter of a function type,
 * or 'mixed' if the function type has no 'this' parameter.
 */
type ThisParameterType<T> = T extends (this: infer U, ...args: empty) => mixed ? U : mixed;

/**
 * Removes the 'this' parameter from a function type.
 */
type OmitThisParameter<T> = mixed extends ThisParameterType<T> ? T : T extends (...args: infer A) => infer R ? (...args: A) => R : T;

type Awaited<T> = T extends Promise<infer U> ? U : T;

/**
 * Extract specific fields from an object, e.g. Pick<O, 'foo' | 'bar'>
 */
type Pick<O: interface {}, Keys: $Keys<O>> = {[key in Keys]: O[key]};

/**
 * Omit specific fields from an object, e.g. Omit<O, 'foo' | 'bar'>
 */
type Omit<O: interface {}, Keys: $Keys<O>> = $Omit<O, Keys>;

/**
 * Construct an object type using string literals as keys with the given type,
 * e.g. Record<'foo' | 'bar', number> = {foo: number, bar: number}
 */
type Record<K: string, T> = {
    [_key in K]: T;
};

// Unused if `experimental.ts_syntax` is off
type ReadonlyMap<K, +V> = $ReadOnlyMap<K, V>;
// Unused if `experimental.ts_syntax` is off
type ReadonlySet<+V> = $ReadOnlySet<V>;

/**
 * The possible underlying representation types of a Flow Enum.
 * For example, `enum E {A = 1}` has the representation type `number`.
 */
type EnumRepresentationTypes = string | number | symbol | boolean | bigint;

/**
 * A generic Flow Enum value. `EnumValue<>` represents any Flow Enum value.
 * You can supply a type argument to restrict to Flow Enums of a certain
 * representation type. For example:
 * ```
 * enum E {A = 1}
 * const b: EnumValue<number> = E.A; // Works
 * const a: EnumValue<> = E.A; // Works
 * ```
 */
type EnumValue<
  +TRepresentationType: EnumRepresentationTypes = EnumRepresentationTypes
> = $EnumValue<TRepresentationType>;

type $EnumValueProto<TEnumObject, TRepresentationType> = {|
  /**
   * Casts the Flow Enum value to its representation type.
   */
  valueOf(this: TEnumObject): TRepresentationType,
  __proto__: null,
|};

/**
 * Represents a generic Flow Enum - the enum itself rather than its values.
 * You can supply a type argument to restrict to Flow Enums with a certain
 * enum value (take a look at the `EnumValue` type for more).
 * You can use the Flow Enum methods like `.cast` and `.members` on a value
 * of this type, but it does not have any specific members which you can access.
 */
type Enum<+TEnumValue: EnumValue<> = EnumValue<>> = $Enum<TEnumValue>;

type Object$Assign = (target: any, ...source: $ReadOnlyArray<any>) => any;
