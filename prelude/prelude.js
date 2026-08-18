/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare var undefined: void;

type PropertyDescriptor<T> = any;

declare class Object {}

declare class Function {}

declare class Boolean {}

declare class Number {}

declare class String {
  @@iterator(): Iterator<string>;
}

declare class RegExp {}

declare class $ReadOnlyArray<out T> {
  @@iterator(): Iterator<T>;
}

declare class Array<T> extends $ReadOnlyArray<T> {
  constructor(arrayLength?: number): void;
}

type $ArrayLike<T> = {
  readonly [indexer: number]: T,
  readonly length: number,
  ...
};

interface TaggedTemplateLiteralArray extends $ReadOnlyArray<string> {
  readonly raw: $ReadOnlyArray<string>;
}

// Promise

declare class Promise<out R> {}

// Iterable/Iterator/Generator

interface $Iterator<out Yield,out Return,in Next> {
  @@iterator(): $Iterator<Yield,Return,Next>;
}
interface $Iterable<out Yield,out Return,in Next> {
  @@iterator(): $Iterator<Yield,Return,Next>;
}
interface Generator<out Yield,out Return,in Next> {
  @@iterator(): $Iterator<Yield,Return,Next>;
}

type Iterator<out T> = $Iterator<T,void,void>;
type Iterable<out T> = $Iterable<T,void,void>;

declare function $iterate<T>(p: Iterable<T>): T;

// Async Iterable/Iterator/Generator

interface $AsyncIterator<out Yield,out Return,in Next> {
  @@asyncIterator(): $AsyncIterator<Yield,Return,Next>;
}
interface $AsyncIterable<out Yield,out Return,in Next> {
  @@asyncIterator(): $AsyncIterator<Yield,Return,Next>;
}
interface AsyncGenerator<out Yield,out Return,in Next> {
  @@asyncIterator(): $AsyncIterator<Yield,Return,Next>;
}

/* Type used internally for inferring the type of the yield delegate */
type $IterableOrAsyncIterableInternal<Input, out Yield, out Return, in Next> =
  Input extends $AsyncIterable<any, any, any>
    ? $AsyncIterable<Yield, Return, Next>
    : $Iterable<Yield, Return, Next>;

type AsyncIterator<out T> = $AsyncIterator<T,void,void>;
type AsyncIterable<out T> = $AsyncIterable<T,void,void>;

declare opaque type $Flow$ModuleRef<out T>;
declare opaque type $Flow$EsmModuleMarkerWrapperInModuleRef<out T>: T;
declare opaque type React$CreateElement;

declare var module: {
  exports: any,
  ...
};

declare var exports: {writeonly [key: string]: mixed};

declare module 'react' {
  type Node = any;
  type RefSetter<T> = any;
}

/**
 * You can use this type instead of `any` to avoid triggering `unclear-type` error.
 * However, it's still a clear signal that you should use a better type.
 */
type $FlowFixMe = any;
