/**
 * Copyright (c) 2013-present, Facebook, Inc.
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

declare function parseInt(string: mixed, radix?: number): number;
declare function parseFloat(string: mixed): number;
declare function isNaN(number: mixed): boolean;
declare function isFinite(number: mixed): boolean;
declare function decodeURI(encodedURI: string): string;
declare function decodeURIComponent(encodedURIComponent: string): string;
declare function encodeURI(uri: string): string;
declare function encodeURIComponent(uriComponent: string): string;

// TODO: instance
declare class Object {
    static (o: ?void): {[key: any]: any};
    static (o: boolean): Boolean;
    static (o: number): Number;
    static (o: string): String;
    static <T: Object>(o: T): T;
    static assign: Object$Assign;
    static create(o: any, properties?: any): any; // compiler magic
    static defineProperties(o: any, properties: any): any;
    static defineProperty(o: any, p: any, attributes: any): any;
    static entries(object: any): Array<[string, mixed]>;
    static freeze<T>(o: T): T;
    static getOwnPropertyDescriptor(o: any, p: any): any;
    static getOwnPropertyNames(o: any): Array<string>;
    static getOwnPropertySymbols(o: any): Symbol[];
    static getPrototypeOf: Object$GetPrototypeOf;
    static is(a: any, b: any): boolean;
    static isExtensible(o: any): boolean;
    static isFrozen(o: any): boolean;
    static isSealed(o: any): boolean;
    static keys(o: any): Array<string>;
    static preventExtensions(o: any): any;
    static seal(o: any): any;
    static setPrototypeOf(o: any, proto: ?Object): bool;
    static values(object: any): Array<mixed>;
    hasOwnProperty(prop: any): boolean;
    propertyIsEnumerable(prop: any): boolean;
    toLocaleString(): string;
    toString(): string;
    valueOf(): Object;
    [key:any]: any;
}

// Well known Symbols.
declare class $SymbolHasInstance mixins Symbol {}
declare class $SymboIsConcatSpreadable mixins Symbol {}
declare class $SymbolIterator mixins Symbol {}
declare class $SymbolMatch mixins Symbol {}
declare class $SymbolReplace mixins Symbol {}
declare class $SymbolSearch mixins Symbol {}
declare class $SymbolSpecies mixins Symbol {}
declare class $SymbolSplit mixins Symbol {}
declare class $SymbolToPrimitive mixins Symbol {}
declare class $SymbolToStringTag mixins Symbol {}
declare class $SymbolUnscopables mixins Symbol {}

declare class Symbol {
  static (value?:any): Symbol;
  static for(key: string): Symbol;
  static hasInstance: $SymbolHasInstance;
  static isConcatSpreadable: $SymboIsConcatSpreadable;
  static iterator: string; // polyfill '@@iterator'
  static keyFor(sym: Symbol): ?string;
  static length: 0;
  static match: $SymbolMatch;
  static replace: $SymbolReplace;
  static search: $SymbolSearch;
  static species: $SymbolSpecies;
  static split: $SymbolSplit;
  static toPrimitive: $SymbolToPrimitive;
  static toStringTag: $SymbolToStringTag;
  static unscopables: $SymbolUnscopables;
  toString(): string;
  valueOf(): ?Symbol;
}

// TODO: instance, static
declare class Function {
      apply: Function$Prototype$Apply; // (thisArg: any, argArray?: any) => any
    bind: Function$Prototype$Bind; // (thisArg: any, ...argArray: Array<any>) => any;
    call: Function$Prototype$Call; // (thisArg: any, ...argArray: Array<any>) => any
    arguments: any;
    caller: Function | null;
    length: number;
    name: string;
}

declare class Boolean {
    static (value:any):boolean;
    valueOf(): boolean;
}

declare class Number {
    static EPSILON: number;
    static MAX_SAFE_INTEGER: number;
    static MAX_VALUE: number;
    static MIN_SAFE_INTEGER: number;
    static MIN_VALUE: number;
    static NaN: number;
    static NEGATIVE_INFINITY: number;
    static POSITIVE_INFINITY: number;
    static (value:any):number;
    static isFinite(value: any): boolean;
    static isInteger(value: any): boolean;
    static isNaN(value: any): boolean;
    static isSafeInteger(value: any): boolean;
    static parseFloat(value: string): number;
    static parseInt(value: string): number;
    toExponential(fractionDigits?: number): string;
    toFixed(fractionDigits?: number): string;
    toPrecision(precision?: number): string;
    toString(radix?: number): string;
    valueOf(): number;
}

declare var Math: {
    E: number;
    LN10: number;
    LN2: number;
    LOG10E: number;
    LOG2E: number;
    PI: number;
    SQRT1_2: number;
    SQRT2: number;
    abs(x: number): number;
    acos(x: number): number;
    acosh(x: number): number;
    asin(x: number): number;
    asinh(x: number): number;
    atan(x: number): number;
    atan2(y: number, x: number): number;
    atanh(x: number): number;
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
    log10(x: number): number;
    log1p(x: number): number;
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
    @@iterator(): Iterator<T>;
    toLocaleString(): string;
    // concat creates a new array
    concat<S, Item: Array<S> | S>(...items: Array<Item>): Array<T | S>;
    copyWithin(target: number, start: number, end?: number): T[];
    entries(): Iterator<[number, T]>;
    every(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): boolean;
    fill(value: T, begin?: number, end?: number): Array<T>;
    filter(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): Array<T>;
    find(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): T;
    findIndex(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): number;
    forEach(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): void;
    includes(searchElement: T, fromIndex?: number): boolean;
    indexOf(searchElement: T, fromIndex?: number): number;
    join(separator?: string): string;
    keys(): Iterator<number>;
    lastIndexOf(searchElement: T, fromIndex?: number): number;
    map<U>(callbackfn: (value: T, index: number, array: Array<T>) => U, thisArg?: any): Array<U>;
    pop(): T;
    push(...items: Array<T>): number;
    reduce<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Array<T>) => U,
      initialValue: U
    ): U;
    reduce<U>(
      callbackfn: (previousValue: T|U, currentValue: T, currentIndex: number, array: Array<T>) => U
    ): U;
    reduceRight<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Array<T>) => U,
      initialValue: U
    ): U;
    reduceRight<U>(
      callbackfn: (previousValue: T|U, currentValue: T, currentIndex: number, array: Array<T>) => U
    ): U;
    reverse(): Array<T>;
    shift(): T;
    slice(start?: number, end?: number): Array<T>;
    some(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): boolean;
    sort(compareFn?: (a: T, b: T) => number): Array<T>;
    splice(start: number, deleteCount?: number, ...items: Array<T>): Array<T>;
    unshift(...items: Array<T>): number;
    values(): Iterator<T>;
    length: number;
    static (...values:Array<any>): Array<any>;
    static isArray(obj: any): bool;
    static from<A, B>(iter: Iterable<A>, mapFn: (elem: A, index: number) => B, thisArg?: any): Array<B>;
    static from<A>(iter: Iterable<A>, mapFn: void): Array<A>;
    static from<A, B>(iter: Iterator<A>, mapFn: (elem: A, index: number) => B, thisArg?: any): Array<B>;
    static from<A>(iter: Iterator<A>, mapFn: void): Array<A>;
    static from<A>(arrayLike: {length: number}, mapFn: (elem: void, index: number) => A, thisArg?: any): Array<A>;
    static from(arrayLike: {length: number}, mapFn: void): Array<void>;
    static of(...values: any[]): any[];
}

declare class String {
    @@iterator(): Iterator<string>;
    anchor(name: string): string;
    charAt(pos: number): string;
    charCodeAt(index: number): number;
    codePointAt(index: number): number;
    concat(...strings: Array<string>): string;
    endsWith(searchString: string, position?: number): boolean;
    includes(searchString: string, position?: number): boolean;
    indexOf(searchString: string, position?: number): number;
    lastIndexOf(searchString: string, position?: number): number;
    link(href: string): string;
    localeCompare(that: string): number;
    match(regexp: string | RegExp): ?Array<string>;
    normalize(format?: string): string;
    repeat(count: number): string;
    replace(searchValue: string | RegExp, replaceValue: string | (substring: string, ...args: Array<any>) => string): string;
    search(regexp: string | RegExp): number;
    slice(start?: number, end?: number): string;
    split(separator: string | RegExp, limit?: number): Array<string>;
    startsWith(searchString: string, position?: number): boolean;
    substr(from: number, length?: number): string;
    substring(start: number, end?: number): string;
    toLocaleLowerCase(): string;
    toLocaleUpperCase(): string;
    toLowerCase(): string;
    toUpperCase(): string;
    trim(): string;
    trimLeft(): string;
    trimRight(): string;
    valueOf(): string;
    length: number;
    static (value:any):string;
    static fromCharCode(...codes: Array<number>): string;
    static fromCodePoint(...codes: Array<number>): string;
    static raw(templateString: string): string;
    static raw(callSite: $Shape<{raw: string}>, ...substitutions: any[]): string;
}

type RegExp$flags =
  'i' | 'g' | 'm' | 'y' | 'ig' | 'im' | 'iy' | 'gi' | 'gm' | 'gy' | 'mi' | 'mg' | 'my' | 'yg' | 'ym' | 'yi' |
  'igm' | 'igy' | 'img' | 'imy' | 'iym' | 'iyg' | 'gim' | 'gym' | 'gmi' | 'gmy' | 'gyi' | 'giy' |
  'mig' | 'mgi' | 'myg' | 'mgy' | 'miy' | 'myi' | 'igmy' | 'igym' | 'imgy' | 'imyg' | 'iygm' |
  'iymg' | 'giym' | 'gimy' | 'gmyi' | 'gmiy' | 'gymi' | 'gyim' | 'migy' | 'miyg' | 'mgiy' |
  'mgyi' | 'myig' | 'mygi' | 'yimg' | 'yigm' | 'ygmi' | 'ygim' | 'ymgi' | 'ymig';

declare class RegExp {
    static (pattern: string | RegExp, flags?: RegExp$flags): RegExp;
    compile(): RegExp;
    constructor(pattern: string | RegExp, flags?: RegExp$flags): RegExp;
    exec(string: string): any;
    flags: string;
    global: boolean;
    ignoreCase: boolean;
    lastIndex: number;
    multiline: boolean;
    source: string;
    sticky: bool;
    unicode: bool;
    test(string: string): boolean;
    toString(): string;
}

declare class Date {
    // new Date();
    // new Date(timestamp);
    // new Date(dateString);
    // new Date(year, month[, day[, hour[, minute[, second[, millisecond]]]]]);
    // TODO: This should specify an overloaded constructor once they're
    // supported, instead of a union type for the first argument.
    constructor(value?: number | string, month?: number, day?: number, hour?: number, minute?: number, second?: number, millisecond?: number): void;
    getDate(): number;
    getDay(): number;
    getFullYear(): number;
    getHours(): number;
    getMilliseconds(): number;
    getMinutes(): number;
    getMonth(): number;
    getSeconds(): number;
    getTime(): number;
    getTimezoneOffset(): number;
    getUTCDate(): number;
    getUTCDay(): number;
    getUTCFullYear(): number;
    getUTCHours(): number;
    getUTCMilliseconds(): number;
    getUTCMinutes(): number;
    getUTCMonth(): number;
    getUTCSeconds(): number;
    setDate(date: number): number;
    setFullYear(year: number, month?: number, date?: number): number;
    setHours(hours: number, min?: number, sec?: number, ms?: number): number;
    setMilliseconds(ms: number): number;
    setMinutes(min: number, sec?: number, ms?: number): number;
    setMonth(month: number, date?: number): number;
    setSeconds(sec: number, ms?: number): number;
    setTime(time: number): number;
    setUTCDate(date: number): number;
    setUTCFullYear(year: number, month?: number, date?: number): number;
    setUTCHours(hours: number, min?: number, sec?: number, ms?: number): number;
    setUTCMilliseconds(ms: number): number;
    setUTCMinutes(min: number, sec?: number, ms?: number): number;
    setUTCMonth(month: number, date?: number): number;
    setUTCSeconds(sec: number, ms?: number): number;
    toDateString(): string;
    toISOString(): string;
    toJSON(key?: any): string;
    toLocaleDateString(): string;
    toLocaleString(): string;
    toLocaleTimeString(): string;
    toTimeString(): string;
    toUTCString(): string;
    valueOf(): number;

    static ():string;
    static now(): number;
    static parse(s: string): number;
    static UTC(year: number, month: number, date?: number, hours?: number, minutes?: number, seconds?: number, ms?: number): number;
    // multiple indexers not yet supported
    [key: $SymbolToPrimitive]: (hint: 'string' | 'default' | 'number') => string | number;
}

declare class Error {
    static (message?:string):Error;
    name: string;
    message: string;
    stack: string;

    // note: v8 only (node/chrome)
    static captureStackTrace(target: Object, constructor?: Function): void;
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
    static stringify(
      value: any,
      replacer?: ?((key: string, value: any) => any) | Array<any>,
      space?: string | number
    ): string;
}

/* Iterators */
type IteratorResult<Yield,Return> = {
  done: true,
  value?: Return,
} | {
  done: false,
  value: Yield,
};

interface $Iterator<Yield,Return,Next> {
    @@iterator(): $Iterator<Yield,Return,Next>;
    next(value?: Next): IteratorResult<Yield,Return>;
}
type Iterator<T> = $Iterator<T,void,void>;

interface $Iterable<Yield,Return,Next> {
    @@iterator(): $Iterator<Yield,Return,Next>;
}
type Iterable<T> = $Iterable<T,void,void>;

/* Generators */
interface Generator<+Yield,+Return,-Next> {
    @@iterator(): $Iterator<Yield,Return,Next>;
    next(value?: Next): IteratorResult<Yield,Return>;
    return<R>(value: R): { done: true, value: R };
    throw(error?: any): IteratorResult<Yield,Return>;
}

/* Maps and Sets */

declare class Map<K, V> {
    @@iterator(): Iterator<[K, V]>;
    constructor<Key, Value>(_: void): Map<Key, Value>;
    constructor<Key, Value>(_: null): Map<Key, Value>;
    constructor<Key, Value>(iterable: Array<[Key, Value]>): Map<Key, Value>;
    constructor<Key, Value>(iterable: Iterable<[Key, Value]>): Map<Key, Value>;
    clear(): void;
    delete(key: K): boolean;
    entries(): Iterator<[K, V]>;
    forEach(callbackfn: (value: V, index: K, map: Map<K, V>) => mixed, thisArg?: any): void;
    get(key: K): V | void;
    has(key: K): boolean;
    keys(): Iterator<K>;
    set(key: K, value: V): Map<K, V>;
    size: number;
    values(): Iterator<V>;
    // Multiple Indexers not yet supported
    [key: $SymbolToStringTag | $SymbolSpecies]: Function;
}

declare class WeakMap<K, V> {
    clear(): void;
    delete(key: K): boolean;
    get(key: K): V;
    has(key: K): boolean;
    set(key: K, value: V): WeakMap<K, V>;
}

declare class Set<T> {
    @@iterator(): Iterator<T>;
    add(value: T): Set<T>;
    clear(): void;
    delete(value: T): boolean;
    entries(): Iterator<[T, T]>;
    forEach(callbackfn: (value: T, index: T, set: Set<T>) => mixed, thisArg?: any): void;
    has(value: T): boolean;
    keys(): Iterator<T>;
    size: number;
    values(): Iterator<T>;
    [key: $SymbolSpecies]: Function; // This would the Set constructor, can't think of a way to correctly type this
}

declare class WeakSet<T: Object> {
    constructor<V: Object>(_: void): WeakSet<V>;
    constructor<V: Object>(iterable: Array<V>): WeakSet<V>;
    constructor<V: Object>(iterable: Iterable<V>): WeakSet<V>;
    add(value: T): WeakSet<T>;
    delete(value: T): boolean;
    has(value: T): boolean;
}

/* Promises
   cf. https://github.com/borisyankov/DefinitelyTyped/blob/master/es6-promises/es6-promises.d.ts
*/

declare class Promise<+R> {
    constructor(callback: (
      resolve: (result: Promise<R> | R) => void,
      reject:  (error: any) => void
    ) => mixed): void;

    then<U>(
      onFulfill?: (value: R) => Promise<U> | U,
      onReject?: (error: any) => Promise<U> | U
    ): Promise<U>;

    catch<U>(
      onReject?: (error: any) => ?Promise<U> | U
    ): Promise<U>;

    static resolve<T>(object?: Promise<T> | T): Promise<T>;
    static reject<T>(error?: any): Promise<T>;
    static all: Promise$All;
    static race<T, Elem: Promise<T> | T>(promises: Array<Elem>): Promise<T>;
}

// we use this signature when typing await expressions
declare function $await<T>(p: Promise<T> | T): T;

/* Binary data */

declare class ArrayBuffer {
    static isView(arg: mixed): boolean;
    constructor(byteLength: number): void;
    byteLength: number;
    slice(begin: number, end?: number): this;
    [key: $SymbolSpecies]: Function; // This would be the constructor, can't think of a way to correctly type this
}

// This is a helper type to simplify the specification, it isn't an interface
// and there are no objects implementing it.
// https://developer.mozilla.org/en-US/docs/Web/API/ArrayBufferView
type $ArrayBufferView = $TypedArray | DataView;

// The TypedArray intrinsic object is a constructor function, but does not have
// a global name or appear as a property of the global object.
// http://www.ecma-international.org/ecma-262/6.0/#sec-%typedarray%-intrinsic-object
declare class $TypedArray {
    static BYTES_PER_ELEMENT: number;
    static from(iterable: Iterable<number>): this;
    static of(...values: number[]): this;

    constructor(length: number): void;
    constructor(typedArray: $TypedArray): void;
    constructor(iterable: Iterable<number>): void;
    constructor(buffer: ArrayBuffer, byteOffset?: number, length?: number): void;

    [index: number]: number;

    @@iterator(): Iterator<number>;

    buffer: ArrayBuffer;
    byteLength: number;
    byteOffset: number;
    length: number;

    copyWithin(target: number, start: number, end?: number): void;
    entries(): Iterator<number>;
    every(callback: (value: number, index: number, array: this) => mixed, thisArg?: any): boolean;
    fill(value: number, start?: number, end?: number): void;
    filter(callback: (value: number, index: number, array: this) => mixed, thisArg?: any): this;
    find(callback: (value: number, index: number, array: this) => mixed, thisArg?: any): number | void;
    findIndex(callback: (value: number, index: number, array: this) => mixed, thisArg?: any): number | void;
    forEach(callback: (value: number, index: number, array: this) => mixed, thisArg?: any): void;
    includes(searchElement: number, fromIndex?: number): boolean;
    indexOf(searchElement: number, fromIndex?: number): number; // -1 if not present
    join(separator?: string): string;
    keys(): Array<number>;
    lastIndexOf(searchElement: number, fromIndex?: number): number; // -1 if not present
    map(callback: (currentValue: number, index: number, array: this) => number, thisArg?: any): this;
    reduce<U>(
      callback: (previousValue: U, currentValue: number, index: number, array: this) => U,
      initialValue: U
    ): U;
    reduce<U>(
      callback: (previousValue: number|U, currentValue: number, index: number, array: this) => U,
      initialValue: void
    ): U;
    reduceRight<U>(
      callback: (previousValue: U, currentValue: number, index: number, array: this) => U,
      initialValue: U
    ): U;
    reduceRight<U>(
      callback: (previousValue: number|U, currentValue: number, index: number, array: this) => U,
      initialValue: void
    ): U;
    reverse(): this;
    set(array: Array<number> | $TypedArray, offset?: number): void;
    slice(begin?: number, end?: number): this;
    some(callback: (value: number, index: number, array: this) => mixed, thisArg?: any): boolean;
    sort(compare?: (a: number, b: number) => number): void;
    subarray(begin?: number, end?: number): this;
    values(): Iterator<number>;
}

declare class Int8Array extends $TypedArray {}
declare class Uint8Array extends $TypedArray {}
declare class Uint8ClampedArray extends $TypedArray {}
declare class Int16Array extends $TypedArray {}
declare class Uint16Array extends $TypedArray {}
declare class Int32Array extends $TypedArray {}
declare class Uint32Array extends $TypedArray {}
declare class Float32Array extends $TypedArray {}
declare class Float64Array extends $TypedArray {}

declare class DataView {
    constructor(buffer: ArrayBuffer, byteOffset?: number, length?: number): void;
    buffer: ArrayBuffer;
    byteLength: number;
    byteOffset: number;
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

/* Reflect API */

declare class Reflect {
    static apply(target: Function, thisArg?: any, argumentsList?: Array<any>): any;
    static construct(target: Function, argumentsList?: Array<any>, newTarget?: Function): any;
    static defineProperty(o: any, p: any, attributes: any): boolean;
    static deleteProperty(o: any, p: any): boolean;
    static get(o: any, p: any, receiver?: any): any;
    static getOwnPropertyDescriptor(o: any, p: any): any;
    static getPrototypeOf(o: any): any;
    static has(o: any, p: any): boolean;
    static isExtensible(o: any): boolean;
    static ownKeys(o: any): Array<any>;
    static preventExtensions(o: any): boolean;
    static set(o: any, p: any, value: any, receiver?: any): boolean;
    static setPrototypeOf(o: any, prototype: any): boolean;
}

/* CommonJS */

declare var global: any;

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

/* Commonly available, shared between node and dom */
declare var console: any;
