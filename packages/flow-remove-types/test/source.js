/* @flow */
// @nolint

// Regular import
import {
  Something,
  type SomeType,
  typeof SomeOtherThing
} from 'some-module';

// Regular import with types only
import {
  type SomeType,
  typeof SomeOtherThing
} from 'some-module';

// Mixed default and named type only imports
import DefaultImport,{
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

  // Class Property with default value and variance
  +covariant: number = 42;

  // Class Property
  prop: any;

  // Class Property with variance
  +propCo: number;

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

// Export type *
export type * from 'some-module';

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

// Comment type includes are not emptied out
class MyClass {
  /*:: prop: string; */
}

// Inferred predicate
function inferredPredicateWithType(arg: mixed): boolean %checks {
  return !!arg;
}

function inferredPredicateWithoutType(arg: mixed): %checks {
  return !!arg;
}

// Type guards
function typeGuardFunction(x: mixed): x is boolean {
  return typeof x === "boolean";
}

const typeGuardArrow = (x: mixed): x is boolean => (typeof x === "boolean");

function typeGuardInComments(x /*: mixed */) /*: x is boolean */ {
  return typeof x === "boolean";
}

function typeAssertsFunction1(x: mixed): asserts x is boolean {
  if (typeof x !== "boolean") throw new Error;
}

function typeAssertsFunction2(x: mixed): asserts x {
  if (!x) throw new Error;
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
declare export function x(): void;
declare export default T;

// `this` params

declare function y (this : string) : void
type T = (this : string) => void
function z (this : string) {}
function u (this : string, ...a) {}
function v (this : string
   , ...a) {}
function w (this
  : string

   ,) {}
function x (this
  : string

   ,
   ...a) {}
function i(
  this: X,
) {}
function j(
  this: X,
  a: string
) {}

function jj(
  this: X,
  a: string
) {
  function jjj(this: X, a: string) {}
}

const f = function(this: string) {}
const g = function(this: string, ...a) {}
const h = function(this
: string,
...a) {}
const k = function(this
: string

,) {}
const kk = function(this
: string,
a: string,) {}

// `as` cast
const asAny = 'any' as any;
const asArray = [1, 2, 3] as number[];
const asBigIntLiteral = 1n as 1n;
const asBigInt = 1n as bigint;
const asBooleanLiteral = true as true;
const asBoolean = true as boolean;
const asComponent = (() => {}) as component(p: number, o?: string);
const asEmpty = {} as empty;
const asExists = 'exists' as *;
const asFunction = (() => {}) as () => void;
const asGeneric = 'generic' as $NonMaybeType<string>;
const asKeyof = 'a' as keyof { a: string; b: number };
const asMixed = 'mixed' as mixed;
const asNullable = null as ?string;
const asNullLiteral = null as null;
const asNumberLiteral = 1 as 1;
const asNumber = 1 as number;
const asObject = { a: 'a' } as { a: string };
const asStringLiteral = 'literal' as 'literal';
const asString = 'string' as string;
const asSymbol = Symbol('symbol') as symbol;
const asTuple = ['a', 1] as [string, number];
const asTypeof = 'typeof' as typeof asString;
const asUnion = 'union' as string | number;
const asVoid = undefined as void;

type ConditionalType<T> = T extends string ? string : number;
const asConditional = 'conditional' as ConditionalType<string>;

interface InterfaceType { a: string; b: number; }
const asInterface = { a: 'a', b: 1 } as InterfaceType;

type InferType<T> = T extends infer U ? U : never;
const asInfer = 'infer' as InferType<string>;

type IntersectionType = { a: string } & { b: number };
const asIntersection = { a: 'a', b: 1 } as IntersectionType;

const asIndexed = 'indexed' as [string, number][0];

// `as const`
's' as const;
['s'] as const;
