/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* JS primitives
   cf. https://github.com/Microsoft/TypeScript/blob/master/lib/lib.d.ts
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
| {...}
| $ReadOnlyArray<mixed>;

// TODO: instance
declare class Object {
    static (o: ?void): { [key: any]: any, ... };
    static (o: boolean): Boolean;
    static (o: number): Number;
    static (o: string): String;
    static <T>(o: T): T;
    static assign: Object$Assign;
    static create(o: any, properties?: PropertyDescriptorMap): any; // compiler magic
    static defineProperties(o: any, properties: PropertyDescriptorMap): any;
    static defineProperty<T>(o: any, p: any, attributes: PropertyDescriptor<T>): any;
    static entries(object: $NotNullOrVoid): Array<[string, mixed]>;
    static freeze<T>(o: T): T;
    static fromEntries<K, V>(entries: Iterable<[K, V] | {
        '0': K,
        '1': V,
        ...
    }>): { [K]: V, ... };

    static getOwnPropertyDescriptor<T>(o: $NotNullOrVoid, p: any): PropertyDescriptor<T> | void;
    static getOwnPropertyDescriptors(o: {...}): PropertyDescriptorMap;
    // This is documentation only. Object.getOwnPropertyNames is implemented in OCaml code
    // https://github.com/facebook/flow/blob/8ac01bc604a6827e6ee9a71b197bb974f8080049/src/typing/statement.ml#L6308
    static getOwnPropertyNames(o: $NotNullOrVoid): Array<string>;
    static getOwnPropertySymbols(o: $NotNullOrVoid): Array<symbol>;
    static getPrototypeOf: Object$GetPrototypeOf;
    static is<T>(a: T, b: T): boolean;
    static isExtensible(o: $NotNullOrVoid): boolean;
    static isFrozen(o: $NotNullOrVoid): boolean;
    static isSealed(o: $NotNullOrVoid): boolean;
    // This is documentation only. Object.keys is implemented in OCaml code.
    // https://github.com/facebook/flow/blob/8ac01bc604a6827e6ee9a71b197bb974f8080049/src/typing/statement.ml#L6308
    static keys(o: $NotNullOrVoid): Array<string>;
    static preventExtensions<T>(o: T): T;
    static seal<T>(o: T): T;
    static setPrototypeOf<T>(o: T, proto: ?{...}): T;
    static values(object: $NotNullOrVoid): Array<mixed>;
    hasOwnProperty(prop: mixed): boolean;
    isPrototypeOf(o: mixed): boolean;
    propertyIsEnumerable(prop: mixed): boolean;
    toLocaleString(): string;
    toString(): string;
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
  static (value?:any): symbol;
  static for(key: string): symbol;
  +description: string | void;
  static hasInstance: $SymbolHasInstance;
  static isConcatSpreadable: $SymboIsConcatSpreadable;
  static iterator: string; // polyfill '@@iterator'
  static keyFor(sym: symbol): ?string;
  static length: 0;
  static match: $SymbolMatch;
  static matchAll: $SymbolMatchAll;
  static replace: $SymbolReplace;
  static search: $SymbolSearch;
  static species: $SymbolSpecies;
  static split: $SymbolSplit;
  static toPrimitive: $SymbolToPrimitive;
  static toStringTag: $SymbolToStringTag;
  static unscopables: $SymbolUnscopables;
  toString(): string;
  valueOf(): ?symbol;
}

// TODO: instance, static
declare class Function {
    proto apply: Function$Prototype$Apply; // (thisArg: any, argArray?: any) => any
    proto bind: Function$Prototype$Bind; // (thisArg: any, ...argArray: Array<any>) => any;
    proto call: Function$Prototype$Call; // (thisArg: any, ...argArray: Array<any>) => any
    toString(): string;
    arguments: any;
    caller: any | null;
    length: number;
    name: string;
}

declare class Boolean {
    constructor(value?: mixed): void;
    static (value:mixed):boolean;
    valueOf(): boolean;
    toString(): string;
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
    static (value:mixed):number;
    static isFinite(value: mixed): boolean;
    static isInteger(value: mixed): boolean;
    static isNaN(value: mixed): boolean;
    static isSafeInteger(value: mixed): boolean;
    static parseFloat(value: string): number;
    static parseInt(value: string, radix?: number): number;
    constructor(value?: mixed): void;
    toExponential(fractionDigits?: number): string;
    toFixed(fractionDigits?: number): string;
    toLocaleString(locales?: string | Array<string>, options?: Intl$NumberFormatOptions): string;
    toPrecision(precision?: number): string;
    toString(radix?: number): string;
    valueOf(): number;
}

declare var Math: {
    E: number,
    LN10: number,
    LN2: number,
    LOG10E: number,
    LOG2E: number,
    PI: number,
    SQRT1_2: number,
    SQRT2: number,
    abs(x: number): number,
    acos(x: number): number,
    acosh(x: number): number,
    asin(x: number): number,
    asinh(x: number): number,
    atan(x: number): number,
    atan2(y: number, x: number): number,
    atanh(x: number): number,
    cbrt(x: number): number,
    ceil(x: number): number,
    clz32(x: number): number,
    cos(x: number): number,
    cosh(x: number): number,
    exp(x: number): number,
    expm1(x: number): number,
    floor(x: number): number,
    fround(x: number): number,
    hypot(...values: Array<number>): number,
    imul(y: number, x: number): number,
    log(x: number): number,
    log10(x: number): number,
    log1p(x: number): number,
    log2(x: number): number,
    max(...values: Array<number>): number,
    min(...values: Array<number>): number,
    pow(x: number, y: number): number,
    random(): number,
    round(x: number): number,
    sign(x: number): number,
    sin(x: number): number,
    sinh(x: number): number,
    sqrt(x: number): number,
    tan(x: number): number,
    tanh(x: number): number,
    trunc(x: number): number,
    ...
};

/* All the Array.prototype methods and properties that don't mutate the array.
 */
declare class $ReadOnlyArray<+T> {
    @@iterator(): Iterator<T>;
    toLocaleString(): string;
    // concat creates a new array
    concat<S, Item: $ReadOnlyArray<S> | S>(...items: Array<Item>): Array<T | S>;
    entries(): Iterator<[number, T]>;
    every(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg?: any): boolean;
    filter(callbackfn: typeof Boolean): Array<$NonMaybeType<T>>;
    filter(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg?: any): Array<T>;
    find(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg?: any): T | void;
    findIndex(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg?: any): number;
    forEach(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg?: any): void;
    includes(searchElement: mixed, fromIndex?: number): boolean;
    indexOf(searchElement: mixed, fromIndex?: number): number;
    join(separator?: string): string;
    keys(): Iterator<number>;
    lastIndexOf(searchElement: mixed, fromIndex?: number): number;
    map<U>(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => U, thisArg?: any): Array<U>;
    flatMap<U>(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => $ReadOnlyArray<U> | U, thisArg?: any): Array<U>;
    flat(depth?: number): Array<mixed>;
    reduce(
      callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: $ReadOnlyArray<T>) => T,
    ): T;
    reduce<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: $ReadOnlyArray<T>) => U,
      initialValue: U
    ): U;
    reduceRight(
      callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: $ReadOnlyArray<T>) => T,
    ): T;
    reduceRight<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: $ReadOnlyArray<T>) => U,
      initialValue: U
    ): U;
    slice(start?: number, end?: number): Array<T>;
    some(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg?: any): boolean;
    values(): Iterator<T>;
    +[key: number]: T;
    +length: number;
}

declare class Array<T> extends $ReadOnlyArray<T> {
    copyWithin(target: number, start: number, end?: number): T[];
    every(callbackfn: (value: T, index: number, array: Array<T>) => mixed, thisArg?: any): boolean;
    fill(value: T, begin?: number, end?: number): Array<T>;
    filter(callbackfn: typeof Boolean): Array<$NonMaybeType<T>>;
    filter(callbackfn: (value: T, index: number, array: Array<T>) => mixed, thisArg?: any): Array<T>;
    find(callbackfn: (value: T, index: number, array: Array<T>) => mixed, thisArg?: any): T | void;
    findIndex(callbackfn: (value: T, index: number, array: Array<T>) => mixed, thisArg?: any): number;
    forEach(callbackfn: (value: T, index: number, array: Array<T>) => mixed, thisArg?: any): void;
    map<U>(callbackfn: (value: T, index: number, array: Array<T>) => U, thisArg?: any): Array<U>;
    flatMap<U>(callbackfn: (value: T, index: number, array: Array<T>) => $ReadOnlyArray<U> | U, thisArg?: any): Array<U>;
    flat(depth?: number): Array<mixed>;
    pop(): T;
    push(...items: Array<T>): number;
    reduce(
      callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: Array<T>) => T,
    ): T;
    reduce<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Array<T>) => U,
      initialValue: U
    ): U;
    reduceRight(
      callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: Array<T>) => T,
    ): T;
    reduceRight<U>(
      callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Array<T>) => U,
      initialValue: U
    ): U;
    reverse(): Array<T>;
    shift(): T;
    some(callbackfn: (value: T, index: number, array: Array<T>) => mixed, thisArg?: any): boolean;
    sort(compareFn?: (a: T, b: T) => number): Array<T>;
    splice(start: number, deleteCount?: number, ...items: Array<T>): Array<T>;
    unshift(...items: Array<T>): number;


    [key: number]: T;
    length: number;
    static (...values:Array<any>): Array<any>;
    static isArray(obj: mixed): bool;
    static from<A, B>(iter: Iterable<A>, mapFn: (elem: A, index: number) => B, thisArg?: any): Array<B>;
    static from<A>(iter: Iterable<A>, mapFn: void): Array<A>;
    static from<A, B>(iter: Iterator<A>, mapFn: (elem: A, index: number) => B, thisArg?: any): Array<B>;
    static from<A>(iter: Iterator<A>, mapFn: void): Array<A>;
    static from<A>(arrayLike: { length: number, ... }, mapFn: (elem: void, index: number) => A, thisArg?: any): Array<A>;
    static from(arrayLike: { length: number, ... }, mapFn: void): Array<void>;
    static of<T>(...values: Array<T>): Array<T>;
}

type $ArrayLike<T> = {
  [indexer: number]: T,
  length: number,
  ...
}

type RegExp$flags = $CharSet<"gimsuy">;
type RegExp$matchResult = Array<string> & {
    index: number,
    input: string,
    groups: ?{ [name: string]: string, ... },
    ...
};

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
    localeCompare(that: string, locales?: string | Array<string>, options?: Intl$CollatorOptions): number;
    match(regexp: string | RegExp): RegExp$matchResult | null;
    matchAll(regexp: string | RegExp): Iterator<RegExp$matchResult>;
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
    toLocaleLowerCase(locale?: string | Array<string>): string;
    toLocaleUpperCase(locale?: string | Array<string>): string;
    toLowerCase(): string;
    toUpperCase(): string;
    trim(): string;
    trimEnd(): string;
    trimLeft(): string;
    trimRight(): string;
    trimStart(): string;
    valueOf(): string;
    toString(): string;
    length: number;
    [key: number]: string;
    static (value:mixed):string;
    static fromCharCode(...codes: Array<number>): string;
    static fromCodePoint(...codes: Array<number>): string;
    static raw(templateString: string): string;
    static raw(callSite: $Shape<{ raw: string, ... }>, ...substitutions: any[]): string;
}

declare class RegExp {
    static (pattern: string | RegExp, flags?: RegExp$flags): RegExp;
    compile(): RegExp;
    constructor(pattern: string | RegExp, flags?: RegExp$flags): void;
    exec(string: string): RegExp$matchResult | null;
    flags: string;
    global: boolean;
    ignoreCase: boolean;
    lastIndex: number;
    multiline: boolean;
    source: string;
    sticky: boolean;
    unicode: boolean;
    dotAll: boolean;
    test(string: string): boolean;
    toString(): string;
    +[key: $SymbolMatch | $SymbolMatchAll]: (str: string) => Iterator<RegExp$matchResult>
}

declare class Date {
    constructor(): void;
    constructor(timestamp: number): void;
    constructor(date: Date): void;
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
    toJSON(key?: mixed): string;
    toLocaleDateString(locales?: string | Array<string>, options?: Intl$DateTimeFormatOptions): string;
    toLocaleString(locales?: string | Array<string>, options?: Intl$DateTimeFormatOptions): string;
    toLocaleTimeString(locales?: string | Array<string>, options?: Intl$DateTimeFormatOptions): string;
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
    getFunction(): ?((...any) => any);
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
    static captureStackTrace(target: {[any] : any, ...}, constructor?: any): void;

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

declare class JSON {
    static parse(text: string, reviver?: (key: any, value: any) => any): any;
    static stringify(
      value: null | string | number | boolean | {...} | $ReadOnlyArray<mixed>,
      replacer?: ?((key: string, value: any) => any) | Array<any>,
      space?: string | number
    ): string;
    static stringify(
      value: mixed,
      replacer?: ?((key: string, value: any) => any) | Array<any>,
      space?: string | number
    ): string | void;
}

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

declare class $ReadOnlyMap<K, +V> {
    @@iterator(): Iterator<[K, V]>;
    entries(): Iterator<[K, V]>;
    forEach(callbackfn: (value: V, index: K, map: $ReadOnlyMap<K, V>) => mixed, thisArg?: any): void;
    get(key: K): V | void;
    has(key: K): boolean;
    keys(): Iterator<K>;
    size: number;
    values(): Iterator<V>;
}

declare class Map<K, V> extends $ReadOnlyMap<K, V> {
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
    +[key: $SymbolToStringTag]: any;
    static +[key: $SymbolSpecies]: any;
}

declare class $ReadOnlyWeakMap<K: {...} | $ReadOnlyArray<mixed>, +V> {
    get(key: K): V | void;
    has(key: K): boolean;
}

declare class WeakMap<K: {...} | $ReadOnlyArray<mixed>, V> extends $ReadOnlyWeakMap<K, V> {
    constructor(iterable: ?Iterable<[K, V]>): void;
    delete(key: K): boolean;
    get(key: K): V | void;
    has(key: K): boolean;
    set(key: K, value: V): WeakMap<K, V>;
}

declare class $ReadOnlySet<T> {
    @@iterator(): Iterator<T>;
    entries(): Iterator<[T, T]>;
    forEach(callbackfn: (value: T, index: T, set: $ReadOnlySet<T>) => mixed, thisArg?: any): void;
    has(value: T): boolean;
    keys(): Iterator<T>;
    size: number;
    values(): Iterator<T>;
}

declare class Set<T> extends $ReadOnlySet<T> {
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
    +[key: $SymbolToStringTag]: (...any) => any;
    static +[key: $SymbolSpecies]: (...any) => any; // This would the Set constructor, can't think of a way to correctly type this
}

declare class $ReadOnlyWeakSet<T: {...} | $ReadOnlyArray<any>> {
    has(value: T): boolean;
}

declare class WeakSet<T: {...} | $ReadOnlyArray<any>> extends $ReadOnlyWeakSet<T> {
    constructor(iterable?: Iterable<T>): void;
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
      reject: (error: any) => void
    ) => mixed): void;

    then(onFulfill: null | void, onReject: null | void): Promise<R>;
    then<U>(
      onFulfill: null | void,
      onReject: (error: any) => Promise<U> | U
    ): Promise<R | U>;
    then<U>(
      onFulfill: (value: R) => Promise<U> | U,
      onReject: null | void | ((error: any) => Promise<U> | U)
    ): Promise<U>;

    catch(onReject: null | void): Promise<R>;
    catch<U>(
      onReject: (error: any) => Promise<U> | U
    ): Promise<R | U>;

    finally(onFinally: () => mixed): Promise<R>;

    static resolve<T>(object: Promise<T> | T): Promise<T>;
    static reject<T>(error: any): Promise<T>;
    static all<T: Iterable<mixed>>(promises: T): Promise<$TupleMap<T, typeof $await>>;
    static race<T, Elem: Promise<T> | T>(promises: Iterable<Elem>): Promise<T>;
}

// we use this signature when typing await expressions
declare function $await<T>(p: Promise<T> | T): T;

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

/* Reflect API */

declare var Reflect: {
    apply(target: (...any) => any, thisArg?: any, argumentsList?: Array<any>): any,
    construct(target: (...any) => any, argumentsList?: Array<any>, newTarget?: any): any,
    defineProperty(o: any, p: any, attributes: any): boolean,
    deleteProperty(o: any, p: any): boolean,
    get(o: any, p: any, receiver?: any): any,
    getOwnPropertyDescriptor(o: any, p: any): any,
    getPrototypeOf: Object$GetPrototypeOf,
    setPrototypeOf: Object$SetPrototypeOf,
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
    builtinModules: Array<string>,
    ...
};
declare var require: {
    (id: string): any,
    resolve: (id: string, options?: { paths?: string[] }) => string,
    cache: any,
    main: typeof module,
    ...
};
declare var exports: any;

/* Opaque type for module reference magic strings */
declare opaque type $Flow$ModuleRef<T>;

/* Commonly available, shared between node and dom */
declare var console: {
    assert(condition: mixed, ...data: Array<any>): void,
    clear(): void,
    count(label: string): void,
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
    time(label: string): void,
    timeEnd(label: string): void,
    timeStamp(label?: string): void,
    trace(...data: Array<any>): void,
    warn(...data: Array<any>): void,
    ...
};

type $EnumProto<TEnum, TRepresentation> = {|
  cast(TRepresentation): void | TEnum,
  isValid(TRepresentation): boolean,
  members(): Iterable<TEnum>,
  __proto__: null,
|}
