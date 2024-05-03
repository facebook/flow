/*  */
// @nolint

// Regular import
import {
  Something,
} from 'some-module';

// Regular import with types only

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
