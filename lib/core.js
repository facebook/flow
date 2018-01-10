/**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
/* JS primitives
   cf. http://typescript.codeplex.com/sourcecontrol/latest#typings/lib.d.ts
*/

declare var NaN: number;
declare var Infinity: number;
declare var undefined: void;

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
    static setPrototypeOf(o: any, proto: ?Object): any;
    static values(object: any): Array<mixed>;
    hasOwnProperty(prop: any): boolean;
    isPrototypeOf(o: any): boolean;
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
    constructor(value?: mixed): void;
    static (value:any):boolean;
    valueOf(): boolean;
    toString(): string;
}

type Number$LocaleOptions = {
  localeMatcher?: string,
  style?: string,
  currency?: string,
  currencyDisplay?: string,
  useGrouping?: boolean,
  minimumIntegerDigits?: number,
  minimumFractionDigits?: number,
  maximumFractionDigits?: number,
  minimumSignificantDigits?: number,
  maximumSignificantDigits?: number,
};

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
    static parseInt(value: string, radix?: number): number;
    constructor(value?: mixed): void;
    toExponential(fractionDigits?: number): string;
    toFixed(fractionDigits?: number): string;
    toLocaleString(locales?: string | Array<string>, options?: Number$LocaleOptions): string;
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

/* All the Array.prototype methods and properties that don't mutate the array.
 */
declare class $ReadOnlyArray<+T> {
    @@iterator(): Iterator<T>;
    toLocaleString(): string;
    // concat creates a new array
    concat<S, Item: $ReadOnlyArray<S> | S>(...items: Array<Item>): Array<T | S>;
    entries(): Iterator<[number, T]>;
    every(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => any, thisArg?: any): boolean;
    filter(callbackfn: typeof Boolean): Array<$NonMaybeType<T>>;
    filter(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => any, thisArg?: any): Array<T>;
    find(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => any, thisArg?: any): T | void;
    findIndex(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => any, thisArg?: any): number;
    forEach(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => any, thisArg?: any): void;
    includes(searchElement: mixed, fromIndex?: number): boolean;
    indexOf(searchElement: mixed, fromIndex?: number): number;
    join(separator?: string): string;
    keys(): Iterator<number>;
    lastIndexOf(searchElement: mixed, fromIndex?: number): number;
    map<U>(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => U, thisArg?: any): Array<U>;

    reduce(
      callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: $ReadOnlyArray<T>) => T,
      initialValue: void
    ): T;
    reduce<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: $ReadOnlyArray<T>) => U,
      initialValue: U
    ): U;
    reduceRight(
      callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: $ReadOnlyArray<T>) => T,
      initialValue: void
    ): T;
    reduceRight<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: $ReadOnlyArray<T>) => U,
      initialValue: U
    ): U;
    slice(start?: number, end?: number): Array<T>;
    some(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => any, thisArg?: any): boolean;
    values(): Iterator<T>;
    +[key: number]: T;
    +length: number;
}

declare class Array<T> extends $ReadOnlyArray<T> {
    copyWithin(target: number, start: number, end?: number): T[];
    every(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): boolean;
    fill(value: T, begin?: number, end?: number): Array<T>;
    filter(callbackfn: typeof Boolean): Array<$NonMaybeType<T>>;
    filter(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): Array<T>;
    find(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): T | void;
    findIndex(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): number;
    forEach(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): void;
    map<U>(callbackfn: (value: T, index: number, array: Array<T>) => U, thisArg?: any): Array<U>;
    pop(): T;
    push(...items: Array<T>): number;
    reduce(
      callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: Array<T>) => T,
      initialValue: void
    ): T;
    reduce<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Array<T>) => U,
      initialValue: U
    ): U;
    reduceRight(
      callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: Array<T>) => T,
      initialValue: void
    ): T;
    reduceRight<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Array<T>) => U,
      initialValue: U
    ): U;
    reverse(): Array<T>;
    shift(): T;
    some(callbackfn: (value: T, index: number, array: Array<T>) => any, thisArg?: any): boolean;
    sort(compareFn?: (a: T, b: T) => number): Array<T>;
    splice(start: number, deleteCount?: number, ...items: Array<T>): Array<T>;
    unshift(...items: Array<T>): number;


    [key: number]: T;
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
    constructor(value?: mixed): void;
    endsWith(searchString: string, position?: number): boolean;
    includes(searchString: string, position?: number): boolean;
    indexOf(searchString: string, position?: number): number;
    lastIndexOf(searchString: string, position?: number): number;
    link(href: string): string;
    localeCompare(that: string, locales?: string | Array<string>, options?: Object): number;
    match(regexp: string | RegExp): ?Array<string>;
    normalize(format?: string): string;
    padEnd(targetLength: number, padString?: string): string;
    padStart(targetLength: number, padString?: string): string;
    repeat(count: number): string;
    replace(searchValue: string | RegExp, replaceValue: string | (substring: string, ...args: Array<any>) => string): string;
    search(regexp: string | RegExp): number;
    slice(start?: number, end?: number): string;
    split(separator?: string | RegExp, limit?: number): Array<string>;
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
    toString(): string;
    length: number;
    static (value:any):string;
    static fromCharCode(...codes: Array<number>): string;
    static fromCodePoint(...codes: Array<number>): string;
    static raw(templateString: string): string;
    static raw(callSite: $Shape<{raw: string}>, ...substitutions: any[]): string;
}

type RegExp$flags = $CharSet<"gimsuy">

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

type Date$LocaleOptions = {
  localeMatcher?: string,
  timeZone?: string,
  hour12?: boolean,
  formatMatcher?: string,
  weekday?: string,
  era?: string,
  year?: string,
  month?: string,
  day?: string,
  hour?: string,
  minute?: string,
  second?: string,
  timeZoneName?: string,
};

declare class Date {
    constructor(): void;
    constructor(timestamp: number): void;
    constructor(dateString: string): void;
    constructor(year: number, month: number, day?: number, hour?: number, minute?: number, second?: number, millisecond?: number): void;
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
    toLocaleDateString(locales?: string | Array<string>, options?: Date$LocaleOptions): string;
    toLocaleString(locales?: string | Array<string>, options?: Date$LocaleOptions): string;
    toLocaleTimeString(locales?: string | Array<string>, options?: Date$LocaleOptions): string;
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

declare class CallSite {
    getThis(): any;
    getTypeName(): string;
    getFunction(): ?Function;
    getFunctionName(): string;
    getMethodName(): string;
    getFileName(): ?string;
    getLineNumber(): ?number;
    getColumnNumber(): ?number;
    getEvalOrigin(): ?CallSite;
    getScriptNameOrSourceURL(): ?string;
    isToplevel(): bool;
    isEval(): bool;
    isNative(): bool;
    isConstructor(): bool;
    toString(): string;
}

declare class Error {
    static (message?:string):Error;
    constructor (message?: mixed): void;
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

    // note: v8 only (node/chrome)
    static captureStackTrace(target: Object, constructor?: Function): void;

    static stackTraceLimit: number;
    static prepareStackTrace: (err: Error, stack: CallSite[]) => mixed;
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

/* Iterable/Iterator/Generator */

type IteratorResult<+Yield,+Return> =
  | { done: true, +value?: Return }
  | { done: false, +value: Yield };

interface $Iterator<+Yield,+Return,-Next> {
    @@iterator(): $Iterator<Yield,Return,Next>;
    next(value?: Next): IteratorResult<Yield,Return>;
}
type Iterator<+T> = $Iterator<T,void,void>;

interface $Iterable<+Yield,+Return,-Next> {
    @@iterator(): $Iterator<Yield,Return,Next>;
}
type Iterable<+T> = $Iterable<T,void,void>;

interface Generator<+Yield,+Return,-Next> {
    @@iterator(): $Iterator<Yield,Return,Next>;
    next(value?: Next): IteratorResult<Yield,Return>;
    return<R>(value: R): IteratorResult<Yield,R|Return>;
    throw(error?: any): IteratorResult<Yield,Return>;
}

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

declare function $asyncIterator<T>(p: AsyncIterable<T>): T;

/* Maps and Sets */

declare class Map<K, V> {
    @@iterator(): Iterator<[K, V]>;
    constructor(iterable: ?Iterable<[K, V]>): void;
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
    constructor(iterable: ?Iterable<[K, V]>): void;
    delete(key: K): boolean;
    get(key: K): V | void;
    has(key: K): boolean;
    set(key: K, value: V): WeakMap<K, V>;
}

declare class Set<T> {
    @@iterator(): Iterator<T>;
    constructor(iterable: ?Iterable<T>): void;
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
    constructor(iterable?: Iterable<T>): void;
    add(value: T): WeakSet<T>;
    delete(value: T): boolean;
    has(value: T): boolean;
}

// For backwards compatibility make error type optional with default being mixed.
// That way `onReject` handler for `Promise<a>` won't be able to assume any type
// instead it will have to refine a type.
declare class Promise<R, E = mixed> {
    constructor(
      callback: (
        resolve: (result: Promise<R, E> | R) => void,
        reject: (error: E) => void
      ) => mixed
    ): void;

    // If both handlers are omitted return promise has the same type as this.
    then(): Promise<R, E>;
    // then(f) can not affect error type of `this` promise neither it can return
    // promise with completely different error type. Instead it can return
    // promise with extended error type. For example if `this` has type
    // `Promise<number, string>` and `onFulfill: number => Promise<number, Error>`
    // is passed return promise will have type of `Promise<number, string|Error>`
    // which makes sense becouse if `this` is rejected then `onFulfill` will never
    // run and there for returne promise will be of type `Promise<empty, string>`
    // but if `this` is fulfilled then `onFulfill` will run and returned promise
    // will have type of `Promise<number, Error>` which is why return promise for
    // either case would be of type `Promise<number, string|Error>`.
    //
    // then(f) can return promise with completely different value type though,
    // consider same `this` promise type `Promise<number, string>` and different
    // `onFulfill: number => string` handler. If this is resolved then returned
    // promise type will be of `Promise<string, empty>` but if `this` is rejected
    // then returned promise will be of type `Promise<empty, string>` there for
    // return type for either case would be `Promise<string, string>`.
    //
    // then(f) could actually do both of the things described above - return a
    // promise with competely different value type and extended error type that is
    // when `onFulfill: string => Promise<string, Error>` returns promise with
    // both different value & different error type. Resulting promise in that
    // case will be hybrid of both cases: `Promise<string, string|Error>` due to
    // reasons explained above.
    then<U, Y>(onFulfill: (value: R) => Promise<U, Y> | U): Promise<U, E | Y>;
    // catch can not affect value type of `this` promise neither it can return
    // promise with a completely different value type instead it can return
    // promise with extended value type. For example if `this` has type
    // `Promise<number, string>` and `onReject: string => string` is passed
    // returned promise type will be `Promise<number|string, any>` which makes sense
    // because if `this` is resolved `onReject` never runs so value type is
    // number. If this is rejected then `onReject` runs and resulting promise will
    // have value of type `string` there for resulting promise value has type of
    // `number|string` also since in both cases returned promise is resolved it's
    // error type `empty` but for convenience it's left open.
    //
    // catch can return promise with a different error type though that is if it
    // returns rejected promise. Let's consider same example but this time around
    // with `onReject: string => Promise<string, Error>` being passed. Returned
    // promise will have type of `Promise<number|string, Error>` which maske sense
    // because if `this` is resolved onReject never runs so returne promise will
    // be of type `Promise<number, empty>`, but if `this` is rejected then return
    // promise wil have type of `Promise<string, Error>` there for resulting
    // promise for either case would be `Promise<number|string, Error>`.
    catch<T, Y>(onReject: (error: E) => Promise<T, Y> | T): Promise<R | T, Y>;
    // Please note that `then(void, f)` is equivalent of `catch` and that is
    // reflected in type signature.
    then<T, Y>(
      onFulfill: void | null,
      onReject: (error: E) => Promise<T, Y> | T
    ): Promise<R | T, Y>;
    // then(f, g) unlike `catch(f)` and `then(f)` can return promise that has
    // completely different value and error types (not just extensions) that is
    // because `value` and `error` types from `this` promise never flow through.
    // Which is why return types of `onFulfill` and `onReject` contain all the
    // info about returned promise type.
    then<U, Y>(
      onFulfill: (value: R) => Promise<U, Y> | U,
      onReject: (error: E) => Promise<U, Y> | U
    ): Promise<U, Y>;

    // Promise.resolve() produces `Promise<void, empty>` but for convenience we
    // keep error type open.
    static resolve<E>(): Promise<void, E>;
    // Promise.resolve(a) returns Promise<a, empty> if `a` isn't a promise and for
    // convenience we keep error type open. If `a` is a promise then resolve
    // returns promise of the same type.
    static resolve<T, E>(object: Promise<T, E> | T): Promise<T, E>;
    // Promise.reject() returns `Promise<empty, void>` but for convenience we keep
    // value type open.
    static reject<T>(): Promise<T, void>;
    // Promise.reject(e) return `Promise<empty, e>` but again for convenience we
    // keep value type open.
    static reject<T, E>(error: E): Promise<T, E>;
  
    static all<T, E>(promises: Array<Promise<T, E>>): Promise<Array<T>, E>;
    static race<T, E>(promises: Array<Promise<T, E>>): Promise<T, E>;
  
    // Non-standard APIs common in some libraries

    done(onFulfill?: (value: R) => mixed, onReject?: (error: E) => mixed): void;

    static cast<E>(): Promise<void, E>;
    static cast<T, E>(object: T): Promise<T, E>;
}

// we use this signature when typing await expressions
declare function $await<T>(p: Promise<T> | T): T;

/* Binary data */

declare class ArrayBuffer {
    static isView(arg: mixed): boolean;
    constructor(byteLength: number): void;
    byteLength: number;
    slice(begin: number, end?: number): this;
    static [key: $SymbolSpecies]: Class<this>;
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
    static from(iterable: Iterable<number>, mapFn?: (element: number) => number, thisArg?: any): this;
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
    entries(): Iterator<[number, number]>;
    every(callback: (value: number, index: number, array: this) => mixed, thisArg?: any): boolean;
    fill(value: number, start?: number, end?: number): this;
    filter(callback: (value: number, index: number, array: this) => mixed, thisArg?: any): this;
    find(callback: (value: number, index: number, array: this) => mixed, thisArg?: any): number | void;
    findIndex(callback: (value: number, index: number, array: this) => mixed, thisArg?: any): number;
    forEach(callback: (value: number, index: number, array: this) => mixed, thisArg?: any): void;
    includes(searchElement: number, fromIndex?: number): boolean;
    indexOf(searchElement: number, fromIndex?: number): number; // -1 if not present
    join(separator?: string): string;
    keys(): Iterator<number>;
    lastIndexOf(searchElement: number, fromIndex?: number): number; // -1 if not present
    map(callback: (currentValue: number, index: number, array: this) => number, thisArg?: any): this;
    reduce(
      callback: (previousValue: number, currentValue: number, index: number, array: this) => number,
      initialValue: void
    ): number;
    reduce<U>(
      callback: (previousValue: U, currentValue: number, index: number, array: this) => U,
      initialValue: U
    ): U;
    reduceRight(
      callback: (previousValue: number, currentValue: number, index: number, array: this) => number,
      initialValue: void
    ): number;
    reduceRight<U>(
      callback: (previousValue: U, currentValue: number, index: number, array: this) => U,
      initialValue: U
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

declare function escape(str: string): string;
declare function unescape(str: string): string;

declare opaque type TimeoutID;
declare opaque type IntervalID;
declare function clearInterval(intervalId?: IntervalID): void;
declare function clearTimeout(timeoutId?: TimeoutID): void;
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

/* Reflect API */

declare class Reflect {
    static apply(target: Function, thisArg?: any, argumentsList?: Array<any>): any;
    static construct(target: Function, argumentsList?: Array<any>, newTarget?: Function): any;
    static defineProperty(o: any, p: any, attributes: any): boolean;
    static deleteProperty(o: any, p: any): boolean;
    static get(o: any, p: any, receiver?: any): any;
    static getOwnPropertyDescriptor(o: any, p: any): any;
    static getPrototypeOf: Object$GetPrototypeOf;
    static setPrototypeOf: Object$SetPrototypeOf;
    static has(o: any, p: any): boolean;
    static isExtensible(o: any): boolean;
    static ownKeys(o: any): Array<any>;
    static preventExtensions(o: any): boolean;
    static set(o: any, p: any, value: any, receiver?: any): boolean;
}

/* Proxy */

type Proxy$traps<T> = {
  getPrototypeOf?: (target: T) => Object|null;
  setPrototypeOf?: (target: T, prototype: Object|null) => boolean;
  isExtensible?: (target: T) => boolean;
  preventExtensions?: (target: T) => boolean;
  getOwnPropertyDescriptor?: (target: T, property: string) => void | Object;
  defineProperty?: (target: T, property: string, descriptor: Object) => boolean;
  has?: (target: T, key: string) => boolean;
  get?: (target: T, property: string, receiver: Proxy<T>) => any;
  set?: (target: T, property: string, value: any, receiver: Proxy<T>) => boolean;
  deleteProperty?: (target: T, property: string) => boolean;
  ownKeys?: (target: T) => Array<string>;
  apply?: (target: T, context: any, args: Array<any>) => any;
  construct?: (target: T, args: Array<any>, newTarget: Function) => Object;
};

type Proxy$revocable<T> = T & {
  revoke(): void;
};

declare class Proxy<T> {
  constructor(target: T, handler: Proxy$traps<T>): T;

  static revocable(target: T, handler: Proxy$traps<T>): Proxy$revocable<T>;
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
declare var require: {
  (id: string): any;
  resolve: (id: string) => string;
  cache: any;
  main: typeof module;
};
declare var exports: any;

/* Commonly available, shared between node and dom */
declare var console: {
  assert(condition: mixed, ...data: Array<any>): void;
  clear(): void;
  count(label: string): void;
  debug(...data: Array<any>): void;
  dir(...data: Array<any>): void;
  dirxml(...data: Array<any>): void;
  error(...data: Array<any>): void;
  _exception(...data: Array<any>): void;
  group(...data: Array<any>): void;
  groupCollapsed(...data: Array<any>): void;
  groupEnd(): void;
  info(...data: Array<any>): void;
  log(...data: Array<any>): void;
  profile(name?: string): void;
  profileEnd(name?: string): void;
  table(tabularData: { [key: string]: any } | Array<{ [key: string]: any }> | Array<Array<any>>): void;
  time(label: string): void;
  timeEnd(label: string): void;
  timeStamp(label?: string): void;
  trace(...data: Array<any>): void;
  warn(...data: Array<any>): void;
};
