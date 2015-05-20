/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */
/* JS primitives
   cf. http://typescript.codeplex.com/sourcecontrol/latest#typings/lib.d.ts
*/

declare var NaN: number;
declare var Infinity: number;

declare function parseInt(string: string, radix?: number): number;
declare function parseFloat(string: string): number;
declare function isNaN(number: number): boolean;
declare function isFinite(number: number): boolean;
declare function decodeURI(encodedURI: string): string;
declare function decodeURIComponent(encodedURIComponent: string): string;
declare function encodeURI(uri: string): string;
declare function encodeURIComponent(uriComponent: string): string;

// TODO: instance
declare class Object {
    static getPrototypeOf(o: any): any; // compiler magic
    static getOwnPropertyDescriptor(o: any, p: any): any;
    static getOwnPropertyNames(o: any): Array<string>;
    static create(o: any, properties?: any): any; // compiler magic
    static defineProperty(o: any, p: any, attributes: any): any;
    static defineProperties(o: any, properties: any): any;
    static seal(o: any): any;
    static freeze<T>(o: T): T;
    static preventExtensions(o: any): any;
    static is(a: any, b: any): boolean;
    static isSealed(o: any): boolean;
    static isFrozen(o: any): boolean;
    static isExtensible(o: any): boolean;
    static keys(o: any): Array<string>;
    static assign(target: any, ...sources: Array<any>): any;
    hasOwnProperty(prop: any): boolean;
    propertyIsEnumerable(prop: any): boolean;
    toLocaleString(): string;
    toString(): string;
    valueOf(): Object;
    [key:any]: any;
}

declare class Symbol {
  static iterator: string; // polyfill '@@iterator'
  static (value?:any): Symbol;
}

// TODO: instance, static
declare class Function {
    apply(thisArg: any, argArray?: any): any;
    call(thisArg: any, ...argArray: Array<any>): any;
    bind(thisArg: any, ...argArray: Array<any>): any;
    arguments: any;
    caller: Function;
}

declare class Boolean {
    static (value:any):boolean;
    valueOf(): boolean;
}

declare class Number {
    toFixed(fractionDigits?: number): string;
    toExponential(fractionDigits?: number): string;
    toPrecision(precision?: number): string;
    toString(radix?: number): string;
    valueOf(): number;
    static isNaN(value: number): boolean;
    static isSafeInteger(value: number): boolean;
    static (value:any):number;
    static EPSILON: number;
    static MAX_SAFE_INTEGER: number;
    static MAX_VALUE: number;
    static MIN_SAFE_INTEGER: number;
    static MIN_VALUE: number;
    static NaN: number;
    static NEGATIVE_INFINITY: number;
    static POSITIVE_INFINITY: number;
}

declare var Math: {
    E: number;
    LN10: number;
    LN2: number;
    LOG2E: number;
    LOG10E: number;
    PI: number;
    SQRT1_2: number;
    SQRT2: number;
    abs(x: number): number;
    acos(x: number): number;
    acosh(x: number): number;
    asin(x: number): number;
    asinh(x: number): number;
    atan(x: number): number;
    atanh(x: number): number;
    atan2(y: number, x: number): number;
    cbrt(x: number): number;
    ceil(x: number): number;
    clz32(x: number): number;
    cos(x: number): number;
    cosh(x: number): number;
    exp(x: number): number;
    expm1(x: number): number;
    floor(x: number): number;
    fround(x: number): number;
    hypot(...values: Array<number>): number;
    imul(y: number, x: number): number;
    log(x: number): number;
    log1p(x: number): number;
    log10(x: number): number;
    log2(x: number): number;
    max(...values: Array<number>): number;
    min(...values: Array<number>): number;
    pow(x: number, y: number): number;
    random(): number;
    round(x: number): number;
    sign(x: number): number;
    sin(x: number): number;
    sinh(x: number): number;
    sqrt(x: number): number;
    tan(x: number): number;
    tanh(x: number): number;
    trunc(x: number): number;
};

declare class Array<T> {
    toLocaleString(): string;
    // concat creates a new array
    concat<S>(...items: Array<Array<S> | S>): Array<T | S>;
    join(separator?: string): string;
    pop(): T;
    push(...items: Array<T>): number;
    reverse(): Array<T>;
    shift(): T;
    slice(start?: number, end?: number): Array<T>;
    sort(compareFn?: (a: T, b: T) => number): Array<T>;
    splice(start: number, deleteCount?: number, ...items: Array<T>): Array<T>;
    unshift(...items: Array<T>): number;
    indexOf(searchElement: T, fromIndex?: number): number;
    includes(searchElement: T, fromIndex?: number): boolean;
    lastIndexOf(searchElement: T, fromIndex?: number): number;
    every(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): boolean;
    some(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): boolean;
    forEach(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): void;
    map<U>(callbackfn: (value: T, index: number, array: Array<T>) => U, thisArg?: any): Array<U>;
    filter(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): Array<T>;
    find(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): T;
    findIndex(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): number;
    reduce<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Array<T>) => U, initialValue: U): U;
    reduceRight<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Array<T>) => U, initialValue: U): U;
    length: number;
    static (...values:Array<any>): Array<any>;
    static isArray(obj: any): bool;
    static from<A, B>(arrayLike: any, mapFn?: ?(elem: A) => B, thisArg?: ?any): Array<B>;
}

declare class String {
    charAt(pos: number): string;
    charCodeAt(index: number): number;
    concat(...strings: Array<string>): string;
    contains(substr: string): boolean;
    indexOf(searchString: string, position?: number): number;
    lastIndexOf(searchString: string, position?: number): number;
    localeCompare(that: string): number;
    match(regexp: string | RegExp): Array<string>;
    startsWith(searchString: string, position?: number): boolean;
    endsWith(searchString: string, position?: number): boolean;
    replace(searchValue: string | RegExp, replaceValue: string | (substring: string, ...args: Array<any>) => string): string;
    search(regexp: string | RegExp): number;
    slice(start?: number, end?: number): string;
    split(separator: string | RegExp, limit?: number): Array<string>;
    substring(start: number, end?: number): string;
    toLowerCase(): string;
    toLocaleLowerCase(): string;
    toUpperCase(): string;
    toLocaleUpperCase(): string;
    trim(): string;
    valueOf(): string;
    length: number;
    substr(from: number, length?: number): string;
    static (value:any):string;
    static fromCharCode(...codes: Array<number>): string;
}

declare class RegExp {
    static (pattern: string, flags?: string): RegExp;
    exec(string: string): any;
    test(string: string): boolean;
    source: string;
    global: boolean;
    ignoreCase: boolean;
    multiline: boolean;
    lastIndex: number;
    compile(): RegExp;
}

declare class Date {
    toDateString(): string;
    toTimeString(): string;
    toLocaleString(): string;
    toLocaleDateString(): string;
    toLocaleTimeString(): string;
    valueOf(): number;
    getTime(): number;
    getFullYear(): number;
    getUTCFullYear(): number;
    getMonth(): number;
    getUTCMonth(): number;
    getDate(): number;
    getUTCDate(): number;
    getDay(): number;
    getUTCDay(): number;
    getHours(): number;
    getUTCHours(): number;
    getMinutes(): number;
    getUTCMinutes(): number;
    getSeconds(): number;
    getUTCSeconds(): number;
    getMilliseconds(): number;
    getUTCMilliseconds(): number;
    getTimezoneOffset(): number;
    setTime(time: number): number;
    setMilliseconds(ms: number): number;
    setUTCMilliseconds(ms: number): number;
    setSeconds(sec: number, ms?: number): number;
    setUTCSeconds(sec: number, ms?: number): number;
    setMinutes(min: number, sec?: number, ms?: number): number;
    setUTCMinutes(min: number, sec?: number, ms?: number): number;
    setHours(hours: number, min?: number, sec?: number, ms?: number): number;
    setUTCHours(hours: number, min?: number, sec?: number, ms?: number): number;
    setDate(date: number): number;
    setUTCDate(date: number): number;
    setMonth(month: number, date?: number): number;
    setUTCMonth(month: number, date?: number): number;
    setFullYear(year: number, month?: number, date?: number): number;
    setUTCFullYear(year: number, month?: number, date?: number): number;
    toUTCString(): string;
    toISOString(): string;
    toJSON(key?: any): string;

    static ():string;
    static parse(s: string): number;
    static UTC(year: number, month: number, date?: number, hours?: number, minutes?: number, seconds?: number, ms?: number): number;
    static now(): number;
}

declare class Error {
    static (message?:string):Error;
    name: string;
    message: string;
    stack: string;
}

declare class EvalError extends Error {
}

declare class RangeError extends Error {
}

declare class ReferenceError extends Error {
}

declare class SyntaxError extends Error {
}

declare class TypeError extends Error {
}

declare class URIError extends Error {
}

declare class JSON {
    static parse(text: string, reviver?: (key: any, value: any) => any): any;
    static stringify(value: any, replacer?: ?((key: string, value: any) => any) | Array<any>, space?: any): string;
}

/* Iterators */
interface IteratorResult<T> {
    done: boolean;
    value?: T;
}

interface Iterator<T> {
    next(): IteratorResult<T>;
}

interface Iterable<T> {
}

/* Maps and Sets */

declare class Map<K, V> {
    <Key, Value>(): Map<Key, Value>;
    <Key, Value>(iterable: Iterable<[Key, Value]>): Map<Key, Value>;
    clear(): void;
    delete(key: K): boolean;
    entries(): Iterator<[K, V]>;
    forEach(callbackfn: (value: V, index: K, map: Map<K, V>) => void, thisArg?: any): void;
    get(key: K): V;
    has(key: K): boolean;
    keys(): Iterator<K>;
    set(key: K, value?: V): Map<K, V>;
    size: number;
    values(): Iterator<V>;
}

declare class WeakMap<K, V> {
    clear(): void;
    delete(key: K): boolean;
    get(key: K): V;
    has(key: K): boolean;
    set(key: K, value: V): WeakMap<K, V>;
}

declare class Set<T> {
    add(value: T): Set<T>;
    clear(): void;
    delete(value: T): boolean;
    forEach(callbackfn: (value: T, index: T, set: Set<T>) => void, thisArg?: any): void;
    has(value: T): boolean;
    size: number;
}

/* Promises
   cf. https://github.com/borisyankov/DefinitelyTyped/blob/master/es6-promises/es6-promises.d.ts
*/

declare class Promise<R> {
    constructor(callback: (
      resolve: (result: Promise<R> | R) => void,
      reject:  (error: any) => void
    ) => void): void;

    then<U>(
      onFulfill?: (value: R) => Promise<U> | U,
      onReject?: (error: any) => Promise<U> | U
    ): Promise<U>;

    done<U>(
      onFulfill?: (value: R) => void,
      onReject?: (error: any) => void
    ): void;

    catch<U>(
      onReject?: (error: any) => ?Promise<U> | U
    ): Promise<U>;

    static resolve<T>(object?: Promise<T> | T): Promise<T>;
    static reject<T>(error?: any): Promise<T>;

    // Non-standard APIs
    static cast<T>(object?: T): Promise<T>;
    static all<T>(promises: Array<Promise<T>>): Promise<Array<T>>;
    static race<T>(promises: Array<Promise<T>>): Promise<T>;
}

/* Binary data */

declare class ArrayBuffer {
    constructor(byteLength: number): void;
    byteLength: number;
    slice(begin:number, end?:number): ArrayBuffer;
}

declare class ArrayBufferView {
    buffer: ArrayBuffer;
    byteOffset: number;
    byteLength: number;
}

declare class Int8Array extends ArrayBufferView {
    // Constructor(unsigned long length),
    // Constructor(TypedArray array),
    // Constructor(type[] array),
    // Constructor(ArrayBuffer buffer, optional unsigned long byteOffset, optional unsigned long length)
    constructor(buffer: ArrayBuffer | number | Array<number> | ArrayBufferView, byteOffset?: number, length?: number): void;
    [index: number]: number;
    BYTES_PER_ELEMENT: number;
    length: number;
    get(index: number): number;
    set(index: number, value: number): void;
    set(array: Int8Array | Array<number>, offset?: number): void;
    subarray(begin: number, end?: number): Int8Array;
}

declare class Uint8Array extends ArrayBufferView {
    constructor(buffer: ArrayBuffer | number | Array<number> | ArrayBufferView, byteOffset?: number, length?: number): void;
    [index: number]: number;
    BYTES_PER_ELEMENT: number;
    length: number;
    get(index: number): number;
    set(index: number, value: number): void;
    set(array: Uint8Array | Array<number>, offset?: number): void;
    subarray(begin: number, end?: number): Uint8Array;
}

declare class Int16Array extends ArrayBufferView {
    constructor(buffer: ArrayBuffer | number | Array<number> | ArrayBufferView, byteOffset?: number, length?: number): void;
    [index: number]: number;
    BYTES_PER_ELEMENT: number;
    length: number;
    get(index: number): number;
    set(index: number, value: number): void;
    set(array: Int16Array | Array<number>, offset?: number): void;
    subarray(begin: number, end?: number): Int16Array;
}

declare class Uint16Array extends ArrayBufferView {
    constructor(buffer: ArrayBuffer | number | Array<number> | ArrayBufferView, byteOffset?: number, length?: number): void;
    [index: number]: number;
    BYTES_PER_ELEMENT: number;
    length: number;
    get(index: number): number;
    set(index: number, value: number): void;
    set(array: Uint16Array | Array<number>, offset?: number): void;
    subarray(begin: number, end?: number): Uint16Array;
}

declare class Int32Array extends ArrayBufferView {
    constructor(buffer: ArrayBuffer | number | Array<number> | ArrayBufferView, byteOffset?: number, length?: number): void;
    [index: number]: number;
    BYTES_PER_ELEMENT: number;
    length: number;
    get(index: number): number;
    set(index: number, value: number): void;
    set(array: Int32Array | Array<number>, offset?: number): void;
    subarray(begin: number, end?: number): Int32Array;
}

declare class Uint32Array extends ArrayBufferView {
    constructor(buffer: ArrayBuffer | number | Array<number> | ArrayBufferView, byteOffset?: number, length?: number): void;
    [index: number]: number;
    BYTES_PER_ELEMENT: number;
    length: number;
    get(index: number): number;
    set(index: number, value: number): void;
    set(array: Uint32Array | Array<number>, offset?: number): void;
    subarray(begin: number, end?: number): Uint32Array;
}

declare class Float32Array extends ArrayBufferView {
    constructor(buffer: ArrayBuffer | number | Array<number> | ArrayBufferView, byteOffset?: number, length?: number): void;
    [index: number]: number;
    BYTES_PER_ELEMENT: number;
    length: number;
    get(index: number): number;
    set(index: number, value: number): void;
    set(array: Float32Array | Array<number>, offset?: number): void;
    subarray(begin: number, end?: number): Float32Array;
}

declare class Float64Array extends ArrayBufferView {
    constructor(buffer: ArrayBuffer | number | Array<number> | ArrayBufferView, byteOffset?: number, length?: number): void;
    [index: number]: number;
    BYTES_PER_ELEMENT: number;
    length: number;
    get(index: number): number;
    set(index: number, value: number): void;
    set(array: Float64Array | Array<number>, offset?: number): void;
    subarray(begin: number, end?: number): Float64Array;
}

declare class DataView extends ArrayBufferView {
    constructor(buffer: ArrayBuffer, byteOffset?: number, length?: number): void;
    getInt8(byteOffset: number): number;
    getUint8(byteOffset: number): number;
    getInt16(byteOffset: number, littleEndian?: boolean): number;
    getUint16(byteOffset: number, littleEndian?: boolean): number;
    getInt32(byteOffset: number, littleEndian?: boolean): number;
    getUint32(byteOffset: number, littleEndian?: boolean): number;
    getFloat32(byteOffset: number, littleEndian?: boolean): number;
    getFloat64(byteOffset: number, littleEndian?: boolean): number;
    setInt8(byteOffset: number, value: number): void;
    setUint8(byteOffset: number, value: number): void;
    setInt16(byteOffset: number, value: number, littleEndian?: boolean): void;
    setUint16(byteOffset: number, value: number, littleEndian?: boolean): void;
    setInt32(byteOffset: number, value: number, littleEndian?: boolean): void;
    setUint32(byteOffset: number, value: number, littleEndian?: boolean): void;
    setFloat32(byteOffset: number, value: number, littleEndian?: boolean): void;
    setFloat64(byteOffset: number, value: number, littleEndian?: boolean): void;
}

declare function btoa(rawString: string): string;
declare function atob(encodedString: string): string;

declare function clearInterval(intervalId?: number): void;
declare function clearTimeout(timeoutId?: any): void;
declare function setTimeout(callback: any, ms?: number, ...args: Array<any>): number;
declare function setInterval(callback: any, ms?: number, ...args: Array<any>): number;

/* NodeJS */

declare var process: any;
declare var global: any;

declare var __filename: string;
declare var __dirname: string;

declare var module: {
    exports: any;
    require(id: string): any;
    id: string;
    filename: string;
    loaded: boolean;
    parent: any;
    children: Array<any>;
};
declare function require(id: string): any;
declare var exports: any;

/* Standard globals */

declare var $: any;
declare function alert(message?: any): void;
declare function prompt(message?: any, value?: any): string;
declare function close(): void;
declare function confirm(message?: string): boolean;
declare var console: any;
declare var event: Event;
declare function getComputedStyle(elt: Element, pseudoElt?: string): any;
declare class Storage {
    length: number;
    getItem(key: string): ?string;
    setItem(key: string, data: string): void;
    clear(): void;
    removeItem(key: string): void;
    key(index: number): ?string;
}
declare var localStorage: Storage;
declare function onfocus(ev: Event): any;
declare function onmessage(ev: Event): any;
declare function open(url?: string, target?: string, features?: string, replace?: boolean): any;
declare var parent: any;
declare function print(): void;
declare var self: any;
declare var sessionStorage: Storage;
declare var status: string;
declare var top: any;
