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
  type SomeOtherType,
  typeof SomeAnotherThing
} from 'some-module';

// Mixed default and named type only imports
import DefaultImport, {
  type SomeDifferentType,
  typeof SomeYetAnotherThing,
} from 'some-module';

// Import types
import type { SomeRandomType } from 'some-module';

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
var SomeClass = class BazClass implements Foo {
  prop: any;

  method(): mixed {
    return;
  }
};

// Parametric class
class Wrapper<ItemType> {
  get(): ItemType {
    return this.value;
  }

  map<MapType>(): Wrapper<MapType> {
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
type StringType = string;

// Export type
export type { StringType };

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
import SomeClassImport from 'some-module'
import type { SomeInterface } from 'some-module'

export class MyClass<GenericType> extends SomeClassImport implements SomeInterface {

  value: GenericType

  constructor(value: GenericType) {
    this.value = value
  }

  get(): GenericType {
    return this.value
  }

}

// Test async/await functions
async function asyncFunction<AsyncType>(input: AsyncType): Promise<AsyncType> {
  return await t;
}

// Test read-only data
export type TestReadOnly = {|
  +readOnly: $ReadOnlyArray<>
|};

// Test covariant type variant class with constaint and default.
export class TestClassWithDefault<+CovariantType: TestReadOnly = TestReadOnly> {

  constructor() {}
}

var newline_arrow = ():
number => 42;

var newline_arrow_2 = () :
number=>42;

// Test calling a function with explicit type arguments
doSomething<number>(3);
doSomething <GenericParam, UnionParam>(3);

// Test invoking a constructor with explicit type arguments
new Event<number>();

// Test type union and intersection syntax with leading "operator"
var union: | UnionType | IntersectionType;
var intersection: & UnionType2 & IntersectionType2;

// Test generic async arrow funcion
const asyncArrow = async <ArrowType>(): ArrowType => {};

// Comment type annotations are preserved
var X /*: {
  version: string,
} */ = { version: '42'};

function method(param /*: string */) /*: number */ {
  // ...
}

// declared class fields
class MyClassWithDeclare {
  declare prop: string;
}

// Comment type includes are not emptied out
class MyClassWithComment {
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
function functionWithDefault<DefaultType, SecondType = DefaultType>() {}

// Opaque types
opaque type OpaqueA = number;
opaque type OpaqueB: string = string;
declare opaque type OpaqueC;
declare opaque type OpaqueD: string;
export opaque type OpaqueE = number;

// Declare export
declare export opaque type OpaqueF;
declare export function exportFunction(): void;
declare export default ExportType;

// `this` params

declare function thisFunction (this : string) : void
type ThisType = (this : string) => void
function thisParam1 (this : string) {}
function thisParam2 (this : string, ...a) {}
function thisParam3 (this : string
   , ...a) {}
function thisParam4 (this
  : string

   ,) {}
function thisParam5 (this
  : string

   ,
   ...a) {}
function thisParam6(
  this: X,
) {}
function thisParam7(
  this: X,
  a: string
) {}

function thisParam8(
  this: X,
  a: string
) {
  function thisParam9(this: X, a: string) {}
}

const thisConst1 = function(this: string) {}
const thisConst2 = function(this: string, ...a) {}
const thisConst3 = function(this
: string,
...a) {}
const thisConst4 = function(this
: string

,) {}
const thisConst5 = function(this
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
const asComponentGeneric = (() => {}) as component<T>(p: T);
const asComponentGenericWithDefault = (() => {}) as component<T = any>(p: T);
const asEmpty = {} as empty;
const asExists = 'exists' as *;
const asFunction = (() => {}) as () => void;
const asKeyof = 'a' as keyof { a: string; b: number };
const asMixed = 'mixed' as mixed;
const asNullable = null as ?string;
const asNullLiteral = null as null;
const asNumberLiteral = 1 as 1;
const asNumber = 1 as number;
const asObject = { a: 'a' } as { a: string };
const asParametrizedGeneric = 'generic' as $NonMaybeType<string>;
const asStringLiteral = 'literal' as 'literal';
const asString = 'string' as string;
const asSymbol = Symbol('symbol') as symbol;
const asTuple = ['a', 1] as [string, number];
const asTypeof = 'typeof' as typeof asString;
const asUnion = 'union' as string | number;
const asVoid = undefined as void;

type ConditionalType<ConditionalParam> = ConditionalParam extends string ? string : number;
const asConditional = 'conditional' as ConditionalType<string>;

interface InterfaceType { a: string; b: number; }
const asInterface = { a: 'a', b: 1 } as InterfaceType;

type InferType<InferParam> = InferParam extends infer InferU ? InferU : never;
const asInfer = 'infer' as InferType<string>;

type IntersectionType = { a: string } & { b: number };
const asIntersection = { a: 'a', b: 1 } as IntersectionType;

const asIndexed = 'indexed' as [string, number][0];

// `as const`
's' as const;
['s'] as const;

// chained `as`
const chain1 = '1' as any as any as any;
const chain2 = '1' as const as any;
