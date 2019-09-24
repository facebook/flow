declare var undefined: void;

type PropertyDescriptor<T> = any

declare class Object {}

declare class Function {}

declare class Boolean {}

declare class Number {}

declare class String {
  @@iterator(): Iterator<string>;
}

declare class RegExp {}

declare class $ReadOnlyArray<+T> {
  @@iterator(): Iterator<T>;
}

declare class Array<T> extends $ReadOnlyArray<T> {}

type $ArrayLike<T> = {
  [indexer: number]: T,
  length: number,
  ...
}

// Promise

declare class Promise<+R> {}
declare function $await<T>(p: Promise<T> | T): T;

// Iterable/Iterator/Generator

interface $Iterator<+Yield,+Return,-Next> {
  @@iterator(): $Iterator<Yield,Return,Next>;
}
interface $Iterable<+Yield,+Return,-Next> {
  @@iterator(): $Iterator<Yield,Return,Next>;
}
interface Generator<+Yield,+Return,-Next> {
  @@iterator(): $Iterator<Yield,Return,Next>;
}

type Iterator<+T> = $Iterator<T,void,void>;
type Iterable<+T> = $Iterable<T,void,void>;

declare function $iterate<T>(p: Iterable<T>): T;

// Async Iterable/Iterator/Generator

interface $AsyncIterator<+Yield,+Return,-Next> {
  @@asyncIterator(): $AsyncIterator<Yield,Return,Next>;
}
interface $AsyncIterable<+Yield,+Return,-Next> {
  @@asyncIterator(): $AsyncIterator<Yield,Return,Next>;
}
interface AsyncGenerator<+Yield,+Return,-Next> {
  @@asyncIterator(): $AsyncIterator<Yield,Return,Next>;
}

type AsyncIterator<+T> = $AsyncIterator<T,void,void>;
type AsyncIterable<+T> = $AsyncIterable<T,void,void>;

declare function $asyncIterator<T>(p: AsyncIterable<T>): T;
