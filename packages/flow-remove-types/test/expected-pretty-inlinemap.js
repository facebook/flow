/*  */
// @nolint

// Regular import
import {
  Something,
} from 'some-module';

// Regular import with types only
import {
} from 'some-module';

// Mixed default and named type only imports
import DefaultImport,{
} from 'some-module';

// Import types

// Typed function
async function test(x, y /*.*/ /*.*/ , z /*.*/ /*.*/ = 123) {
  // Typed expression
  return await (x);
}

// Interface

// Exported interface

// Interface extends

// Implements interface
class Bar extends Other /*.*/ {
  // Class Property with default value
  answer = 42;

  // Class Property with default value and variance
covariant = 42;

  // Class Property
  prop;

  // Class Property with variance
propCo;

  method() {
    return;
  }
}

// Class expression implements interface
var SomeClass = class Baz {
  prop;

  method() {
    return;
  }
};

// Parametric class
class Wrapper {
  get() {
    return this.value;
  }

  map() {
    // do something
  }
}

// Extends Parametric class
class StringWrapper extends Wrapper {
  // ...
}

// Declare class

// Declare funtion

// Declare interface

// Declare module

// Declare type alias

// Declare variable

// Type alias

// Export type

// Export type *

// Regular export
export { Wrapper };

// Exported type alias

// Object with types within
var someObj = {
  objMethod() {
    // do nothing.
  }
}

// Example from README
import SomeClass from 'some-module'

export class MyClass extends SomeClass {

  value

  constructor(value) {
    this.value = value
  }

  get() {
    return this.value
  }

}

// Test async/await functions
async function asyncFunction(input) {
  return await t;
}

// Test read-only data

// Test covariant type variant class with constaint and default.
export class TestClassWithDefault {

  constructor() {}
}

var newline_arrow = () => 42;

var newline_arrow_2 = ()=>42;

// Test calling a function with explicit type arguments
doSomething(3);
doSomething(3);

// Test invoking a constructor with explicit type arguments
new Event();

// Test type union and intersection syntax with leading "operator"
var union;
var intersection;

// Test generic async arrow funcion
const f = async() => {};

// Comment type annotations are preserved
var X /*: {
  version: string,
} */ = { version: '42'};

function method(param /*: string */) /*: number */ {
  // ...
}

// declared class fields
class MyClass {
}

// Comment type includes are not emptied out
class MyClass {
  /*:: prop: string; */
}

// Inferred predicate
function inferredPredicateWithType(arg) {
  return !!arg;
}

function inferredPredicateWithoutType(arg) {
  return !!arg;
}

// Type guards
function typeGuardFunction(x) {
  return typeof x === "boolean";
}

const typeGuardArrow = (x) => (typeof x === "boolean");

function typeGuardInComments(x /*: mixed */) /*: x is boolean */ {
  return typeof x === "boolean";
}

function typeAssertsFunction1(x) {
  if (typeof x !== "boolean") throw new Error;
}

function typeAssertsFunction2(x) {
  if (!x) throw new Error;
}

// Test function with default type parameter
function f() {}

// Opaque types

// Declare export

// `this` params

function z () {}
function u ( ...a) {}
function v (
 ...a) {}
function w (
) {}
function x (
   ...a) {}
function i(
) {}
function j(
  a
) {}

function jj(
  a
) {
  function jjj( a) {}
}

const f = function() {}
const g = function( ...a) {}
const h = function(...a) {}
const k = function(
) {}
const kk = function(a,) {}

// `as` cast
const asAny = 'any';
const asArray = [1, 2, 3];
const asBigIntLiteral = 1n;
const asBigInt = 1n;
const asBooleanLiteral = true;
const asBoolean = true;
const asComponent = (() => {});
const asEmpty = {};
const asExists = 'exists';
const asFunction = (() => {});
const asGeneric = 'generic';
const asKeyof = 'a';
const asMixed = 'mixed';
const asNullable = null;
const asNullLiteral = null;
const asNumberLiteral = 1;
const asNumber = 1;
const asObject = { a: 'a' };
const asStringLiteral = 'literal';
const asString = 'string';
const asSymbol = Symbol('symbol');
const asTuple = ['a', 1];
const asTypeof = 'typeof';
const asUnion = 'union';
const asVoid = undefined;

const asConditional = 'conditional';

const asInterface = { a: 'a', b: 1 };

const asInfer = 'infer';

const asIntersection = { a: 'a', b: 1 };

const asIndexed = 'indexed';

// `as const`
's';
['s'];

//# sourceMappingURL=data:application/json;charset=utf-8;base64,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
