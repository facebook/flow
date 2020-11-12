/* @flow */
// @nolint

// Regular import
import {
  Something,
  type SomeType,
  typeof SomeOtherThing
} from 'some-module';

// Import types
import type { SomeType } from 'some-module';

// Typed function
async function test(x: Type, y /*.*/ ? /*.*/ , z /*.*/ ? /*.*/ : /*.*/ number = 123): string {
  // Typed expression
  return await (x: any);
}

// Interface
interface Foo {
  prop: any;

  method(): mixed;
}

// Exported interface
export interface IThing {
  exported: true;
}

// Interface extends
interface SillyFoo extends Foo {
  silly: string;
}

// Implements interface
class Bar extends Other implements /*.*/ Foo, ISomething {
  // Class Property with default value
  answer: number = 42;

  // Class Property
  prop: any;

  method(): mixed {
    return;
  }
}

// Class expression implements interface
var SomeClass = class Baz implements Foo {
  prop: any;

  method(): mixed {
    return;
  }
};

// Parametric class
class Wrapper<T> {
  get(): T {
    return this.value;
  }

  map<M>(): Wrapper<M> {
    // do something
  }
}

// Extends Parametric class
class StringWrapper extends Wrapper<string> {
  // ...
}

// Declare class
declare class Baz {
  method(): mixed;
}

// Declare funtion
declare function someFunc(): void;

// Declare interface
declare interface ISomething {
  answer: number;
}

// Declare module
declare module 'fs' {
  declare function readThing(path: string): string;
}

// Declare type alias
declare type Location = {
  lat: number,
  lon: number
};

// Declare variable
declare var SOME_CONST: string;

// Type alias
type T = string;

// Export type
export type { T };

// Regular export
export { Wrapper };

// Exported type alias
export type ONE = { one: number };

// Object with types within
var someObj = {
  objMethod(): void {
    // do nothing.
  }
}

// Example from README
import SomeClass from 'some-module'
import type { SomeInterface } from 'some-module'

export class MyClass<T> extends SomeClass implements SomeInterface {

  value: T

  constructor(value: T) {
    this.value = value
  }

  get(): T {
    return this.value
  }

}

// Test async/await functions
async function asyncFunction<T>(input: T): Promise<T> {
  return await t;
}

// Test read-only data
export type TestReadOnly = {|
  +readOnly: $ReadOnlyArray<>
|};

// Test covariant type variant class with constaint and default.
export class TestClassWithDefault<+T: TestReadOnly = TestReadOnly> {

  constructor() {}
}

var newline_arrow = ():
number => 42;

var newline_arrow_2 = () :
number=>42;

// Test calling a function with explicit type arguments
doSomething<number>(3);
doSomething <T, U>(3);

// Test invoking a constructor with explicit type arguments
new Event<number>();

// Test type union and intersection syntax with leading "operator"
var union: | T | U;
var intersection: & T & U;

// Test generic async arrow funcion
const f = async <T>(): T => {};

// Comment type annotations are preserved
var X /*: {
  version: string,
} */ = { version: '42'};

function method(param /*: string */) /*: number */ {
  // ...
}

// declared class fields
class MyClass {
  declare prop: string;
}

// Comment type includes are emptied out
class MyClass {
  /*:: prop: string; */
}

// Inferred predicate
function testit(arg: mixed): boolean %checks {
  return !!arg;
}

// Test function with default type parameter
function f<T, S = T>() {}

// Opaque types
opaque type A = number;
opaque type B: string = string;
declare opaque type A;
declare opaque type B: string;
export opaque type A = number;

// Declare export
declare export opaque type B;
declare export var x;
declare export function x(): void;
declare export default T;

//this params

declare function y (this : string) : void
type T = (this : string) : void
function z (this : string) {}
function u (this : string, ...a) {}

function v (this : string
   , ...a) {}

function w (this
  : string

   ,
   ...a) {}

const f = function(this: string) {}
const g = function(this: string, ...a) {}
const h = function(this
: string,
...a) {}
