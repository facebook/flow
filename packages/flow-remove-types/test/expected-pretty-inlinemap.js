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
import DefaultImport, {
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
