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

//# sourceMappingURL=data:application/json;charset=utf-8;base64,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QUEsQUFBSTs7QUFFaEMsQUFBNkQsQUFBQTtvQ0FDekIsQUFBQSxBQUFFLENBQUMsQUFBQSxBQUF1Qjs7QUFFOUQsQUFBaUQsQUFBQTtxQ0FDWixBQUFBLEFBQUUsQ0FBQyxBQUFBLEFBQWE7O0FBRXJELEFBQWtELEFBQUE7d0JBQzFCLEFBQUEsQUFBRSxDQUFDLEFBQUEsQUFBaUI7O0FBRTVDLEFBQXNELEFBQUE7d0NBQ2QsQUFBQSxBQUFFLENBQUMsQUFBQSxBQUFnQjs7NEJBRS9CLEFBQUEsQUFBRSxDQUFDLEFBQUEsQUFBbUI7OztJQUc5QyxBQUFBLEFBQUUsQ0FBQyxBQUFBLEFBQUs7TUFDTixBQUFBLEFBQUUsQ0FBQyxBQUFBLEFBQUsiLCJzb3VyY2VzQ29udGVudCI6WyIvKiBAZmxvdyAqL1xuLy8gQG5vbGludFxuXG4vLyBSZWd1bGFyIGltcG9ydFxuaW1wb3J0IHtcbiAgU29tZXRoaW5nLFxuICB0eXBlIFNvbWVUeXBlLFxuICB0eXBlb2YgU29tZU90aGVyVGhpbmdcbn0gZnJvbSAnc29tZS1tb2R1bGUnO1xuXG4vLyBSZWd1bGFyIGltcG9ydCB3aXRoIHR5cGVzIG9ubHlcbmltcG9ydCB7XG4gIHR5cGUgU29tZVR5cGUsXG4gIHR5cGVvZiBTb21lT3RoZXJUaGluZ1xufSBmcm9tICdzb21lLW1vZHVsZSc7XG5cbi8vIE1peGVkIGRlZmF1bHQgYW5kIG5hbWVkIHR5cGUgb25seSBpbXBvcnRzXG5pbXBvcnQgRGVmYXVsdEltcG9ydCwge1xuICB0eXBlIFNvbWVUeXBlLFxuICB0eXBlb2YgU29tZU90aGVyVGhpbmdcbn0gZnJvbSAnc29tZS1tb2R1bGUnO1xuXG4vLyBJbXBvcnQgdHlwZXNcbmltcG9ydCB0eXBlIHsgU29tZVR5cGUgfSBmcm9tICdzb21lLW1vZHVsZSc7XG5cbi8vIFR5cGVkIGZ1bmN0aW9uXG5hc3luYyBmdW5jdGlvbiB0ZXN0KHg6IFR5cGUsIHkgLyouKi8gPyAvKi4qLyAsIHogLyouKi8gPyAvKi4qLyA6IC8qLiovIG51bWJlciA9IDEyMyk6IHN0cmluZyB7XG4gIC8vIFR5cGVkIGV4cHJlc3Npb25cbiAgcmV0dXJuIGF3YWl0ICh4OiBhbnkpO1xufVxuXG4vLyBJbnRlcmZhY2VcbmludGVyZmFjZSBGb28ge1xuICBwcm9wOiBhbnk7XG5cbiAgbWV0aG9kKCk6IG1peGVkO1xufVxuXG4vLyBFeHBvcnRlZCBpbnRlcmZhY2VcbmV4cG9ydCBpbnRlcmZhY2UgSVRoaW5nIHtcbiAgZXhwb3J0ZWQ6IHRydWU7XG59XG5cbi8vIEludGVyZmFjZSBleHRlbmRzXG5pbnRlcmZhY2UgU2lsbHlGb28gZXh0ZW5kcyBGb28ge1xuICBzaWxseTogc3RyaW5nO1xufVxuXG4vLyBJbXBsZW1lbnRzIGludGVyZmFjZVxuY2xhc3MgQmFyIGV4dGVuZHMgT3RoZXIgaW1wbGVtZW50cyAvKi4qLyBGb28sIElTb21ldGhpbmcge1xuICAvLyBDbGFzcyBQcm9wZXJ0eSB3aXRoIGRlZmF1bHQgdmFsdWVcbiAgYW5zd2VyOiBudW1iZXIgPSA0MjtcblxuICAvLyBDbGFzcyBQcm9wZXJ0eSB3aXRoIGRlZmF1bHQgdmFsdWUgYW5kIHZhcmlhbmNlXG4gICtjb3ZhcmlhbnQ6IG51bWJlciA9IDQyO1xuXG4gIC8vIENsYXNzIFByb3BlcnR5XG4gIHByb3A6IGFueTtcblxuICAvLyBDbGFzcyBQcm9wZXJ0eSB3aXRoIHZhcmlhbmNlXG4gICtwcm9wQ286IG51bWJlcjtcblxuICBtZXRob2QoKTogbWl4ZWQge1xuICAgIHJldHVybjtcbiAgfVxufVxuXG4vLyBDbGFzcyBleHByZXNzaW9uIGltcGxlbWVudHMgaW50ZXJmYWNlXG52YXIgU29tZUNsYXNzID0gY2xhc3MgQmF6IGltcGxlbWVudHMgRm9vIHtcbiAgcHJvcDogYW55O1xuXG4gIG1ldGhvZCgpOiBtaXhlZCB7XG4gICAgcmV0dXJuO1xuICB9XG59O1xuXG4vLyBQYXJhbWV0cmljIGNsYXNzXG5jbGFzcyBXcmFwcGVyPFQ+IHtcbiAgZ2V0KCk6IFQge1xuICAgIHJldHVybiB0aGlzLnZhbHVlO1xuICB9XG5cbiAgbWFwPE0+KCk6IFdyYXBwZXI8TT4ge1xuICAgIC8vIGRvIHNvbWV0aGluZ1xuICB9XG59XG5cbi8vIEV4dGVuZHMgUGFyYW1ldHJpYyBjbGFzc1xuY2xhc3MgU3RyaW5nV3JhcHBlciBleHRlbmRzIFdyYXBwZXI8c3RyaW5nPiB7XG4gIC8vIC4uLlxufVxuXG4vLyBEZWNsYXJlIGNsYXNzXG5kZWNsYXJlIGNsYXNzIEJheiB7XG4gIG1ldGhvZCgpOiBtaXhlZDtcbn1cblxuLy8gRGVjbGFyZSBmdW50aW9uXG5kZWNsYXJlIGZ1bmN0aW9uIHNvbWVGdW5jKCk6IHZvaWQ7XG5cbi8vIERlY2xhcmUgaW50ZXJmYWNlXG5kZWNsYXJlIGludGVyZmFjZSBJU29tZXRoaW5nIHtcbiAgYW5zd2VyOiBudW1iZXI7XG59XG5cbi8vIERlY2xhcmUgbW9kdWxlXG5kZWNsYXJlIG1vZHVsZSAnZnMnIHtcbiAgZGVjbGFyZSBmdW5jdGlvbiByZWFkVGhpbmcocGF0aDogc3RyaW5nKTogc3RyaW5nO1xufVxuXG4vLyBEZWNsYXJlIHR5cGUgYWxpYXNcbmRlY2xhcmUgdHlwZSBMb2NhdGlvbiA9IHtcbiAgbGF0OiBudW1iZXIsXG4gIGxvbjogbnVtYmVyXG59O1xuXG4vLyBEZWNsYXJlIHZhcmlhYmxlXG5kZWNsYXJlIHZhciBTT01FX0NPTlNUOiBzdHJpbmc7XG5cbi8vIFR5cGUgYWxpYXNcbnR5cGUgVCA9IHN0cmluZztcblxuLy8gRXhwb3J0IHR5cGVcbmV4cG9ydCB0eXBlIHsgVCB9O1xuXG4vLyBFeHBvcnQgdHlwZSAqXG5leHBvcnQgdHlwZSAqIGZyb20gJ3NvbWUtbW9kdWxlJztcblxuLy8gUmVndWxhciBleHBvcnRcbmV4cG9ydCB7IFdyYXBwZXIgfTtcblxuLy8gRXhwb3J0ZWQgdHlwZSBhbGlhc1xuZXhwb3J0IHR5cGUgT05FID0geyBvbmU6IG51bWJlciB9O1xuXG4vLyBPYmplY3Qgd2l0aCB0eXBlcyB3aXRoaW5cbnZhciBzb21lT2JqID0ge1xuICBvYmpNZXRob2QoKTogdm9pZCB7XG4gICAgLy8gZG8gbm90aGluZy5cbiAgfVxufVxuXG4vLyBFeGFtcGxlIGZyb20gUkVBRE1FXG5pbXBvcnQgU29tZUNsYXNzIGZyb20gJ3NvbWUtbW9kdWxlJ1xuaW1wb3J0IHR5cGUgeyBTb21lSW50ZXJmYWNlIH0gZnJvbSAnc29tZS1tb2R1bGUnXG5cbmV4cG9ydCBjbGFzcyBNeUNsYXNzPFQ+IGV4dGVuZHMgU29tZUNsYXNzIGltcGxlbWVudHMgU29tZUludGVyZmFjZSB7XG5cbiAgdmFsdWU6IFRcblxuICBjb25zdHJ1Y3Rvcih2YWx1ZTogVCkge1xuICAgIHRoaXMudmFsdWUgPSB2YWx1ZVxuICB9XG5cbiAgZ2V0KCk6IFQge1xuICAgIHJldHVybiB0aGlzLnZhbHVlXG4gIH1cblxufVxuXG4vLyBUZXN0IGFzeW5jL2F3YWl0IGZ1bmN0aW9uc1xuYXN5bmMgZnVuY3Rpb24gYXN5bmNGdW5jdGlvbjxUPihpbnB1dDogVCk6IFByb21pc2U8VD4ge1xuICByZXR1cm4gYXdhaXQgdDtcbn1cblxuLy8gVGVzdCByZWFkLW9ubHkgZGF0YVxuZXhwb3J0IHR5cGUgVGVzdFJlYWRPbmx5ID0ge3xcbiAgK3JlYWRPbmx5OiAkUmVhZE9ubHlBcnJheTw+XG58fTtcblxuLy8gVGVzdCBjb3ZhcmlhbnQgdHlwZSB2YXJpYW50IGNsYXNzIHdpdGggY29uc3RhaW50IGFuZCBkZWZhdWx0LlxuZXhwb3J0IGNsYXNzIFRlc3RDbGFzc1dpdGhEZWZhdWx0PCtUOiBUZXN0UmVhZE9ubHkgPSBUZXN0UmVhZE9ubHk+IHtcblxuICBjb25zdHJ1Y3RvcigpIHt9XG59XG5cbnZhciBuZXdsaW5lX2Fycm93ID0gKCk6XG5udW1iZXIgPT4gNDI7XG5cbnZhciBuZXdsaW5lX2Fycm93XzIgPSAoKSA6XG5udW1iZXI9PjQyO1xuXG4vLyBUZXN0IGNhbGxpbmcgYSBmdW5jdGlvbiB3aXRoIGV4cGxpY2l0IHR5cGUgYXJndW1lbnRzXG5kb1NvbWV0aGluZzxudW1iZXI+KDMpO1xuZG9Tb21ldGhpbmcgPFQsIFU+KDMpO1xuXG4vLyBUZXN0IGludm9raW5nIGEgY29uc3RydWN0b3Igd2l0aCBleHBsaWNpdCB0eXBlIGFyZ3VtZW50c1xubmV3IEV2ZW50PG51bWJlcj4oKTtcblxuLy8gVGVzdCB0eXBlIHVuaW9uIGFuZCBpbnRlcnNlY3Rpb24gc3ludGF4IHdpdGggbGVhZGluZyBcIm9wZXJhdG9yXCJcbnZhciB1bmlvbjogfCBUIHwgVTtcbnZhciBpbnRlcnNlY3Rpb246ICYgVCAmIFU7XG5cbi8vIFRlc3QgZ2VuZXJpYyBhc3luYyBhcnJvdyBmdW5jaW9uXG5jb25zdCBmID0gYXN5bmMgPFQ+KCk6IFQgPT4ge307XG5cbi8vIENvbW1lbnQgdHlwZSBhbm5vdGF0aW9ucyBhcmUgcHJlc2VydmVkXG52YXIgWCAvKjoge1xuICB2ZXJzaW9uOiBzdHJpbmcsXG59ICovID0geyB2ZXJzaW9uOiAnNDInfTtcblxuZnVuY3Rpb24gbWV0aG9kKHBhcmFtIC8qOiBzdHJpbmcgKi8pIC8qOiBudW1iZXIgKi8ge1xuICAvLyAuLi5cbn1cblxuLy8gZGVjbGFyZWQgY2xhc3MgZmllbGRzXG5jbGFzcyBNeUNsYXNzIHtcbiAgZGVjbGFyZSBwcm9wOiBzdHJpbmc7XG59XG5cbi8vIENvbW1lbnQgdHlwZSBpbmNsdWRlcyBhcmUgbm90IGVtcHRpZWQgb3V0XG5jbGFzcyBNeUNsYXNzIHtcbiAgLyo6OiBwcm9wOiBzdHJpbmc7ICovXG59XG5cbi8vIEluZmVycmVkIHByZWRpY2F0ZVxuZnVuY3Rpb24gaW5mZXJyZWRQcmVkaWNhdGVXaXRoVHlwZShhcmc6IG1peGVkKTogYm9vbGVhbiAlY2hlY2tzIHtcbiAgcmV0dXJuICEhYXJnO1xufVxuXG5mdW5jdGlvbiBpbmZlcnJlZFByZWRpY2F0ZVdpdGhvdXRUeXBlKGFyZzogbWl4ZWQpOiAlY2hlY2tzIHtcbiAgcmV0dXJuICEhYXJnO1xufVxuXG4vLyBUeXBlIGd1YXJkc1xuZnVuY3Rpb24gdHlwZUd1YXJkRnVuY3Rpb24oeDogbWl4ZWQpOiB4IGlzIGJvb2xlYW4ge1xuICByZXR1cm4gdHlwZW9mIHggPT09IFwiYm9vbGVhblwiO1xufVxuXG5jb25zdCB0eXBlR3VhcmRBcnJvdyA9ICh4OiBtaXhlZCk6IHggaXMgYm9vbGVhbiA9PiAodHlwZW9mIHggPT09IFwiYm9vbGVhblwiKTtcblxuZnVuY3Rpb24gdHlwZUd1YXJkSW5Db21tZW50cyh4IC8qOiBtaXhlZCAqLykgLyo6IHggaXMgYm9vbGVhbiAqLyB7XG4gIHJldHVybiB0eXBlb2YgeCA9PT0gXCJib29sZWFuXCI7XG59XG5cbmZ1bmN0aW9uIHR5cGVBc3NlcnRzRnVuY3Rpb24xKHg6IG1peGVkKTogYXNzZXJ0cyB4IGlzIGJvb2xlYW4ge1xuICBpZiAodHlwZW9mIHggIT09IFwiYm9vbGVhblwiKSB0aHJvdyBuZXcgRXJyb3I7XG59XG5cbmZ1bmN0aW9uIHR5cGVBc3NlcnRzRnVuY3Rpb24yKHg6IG1peGVkKTogYXNzZXJ0cyB4IHtcbiAgaWYgKCF4KSB0aHJvdyBuZXcgRXJyb3I7XG59XG5cbi8vIFRlc3QgZnVuY3Rpb24gd2l0aCBkZWZhdWx0IHR5cGUgcGFyYW1ldGVyXG5mdW5jdGlvbiBmPFQsIFMgPSBUPigpIHt9XG5cbi8vIE9wYXF1ZSB0eXBlc1xub3BhcXVlIHR5cGUgQSA9IG51bWJlcjtcbm9wYXF1ZSB0eXBlIEI6IHN0cmluZyA9IHN0cmluZztcbmRlY2xhcmUgb3BhcXVlIHR5cGUgQTtcbmRlY2xhcmUgb3BhcXVlIHR5cGUgQjogc3RyaW5nO1xuZXhwb3J0IG9wYXF1ZSB0eXBlIEEgPSBudW1iZXI7XG5cbi8vIERlY2xhcmUgZXhwb3J0XG5kZWNsYXJlIGV4cG9ydCBvcGFxdWUgdHlwZSBCO1xuZGVjbGFyZSBleHBvcnQgZnVuY3Rpb24geCgpOiB2b2lkO1xuZGVjbGFyZSBleHBvcnQgZGVmYXVsdCBUO1xuXG4vLyBgdGhpc2AgcGFyYW1zXG5cbmRlY2xhcmUgZnVuY3Rpb24geSAodGhpcyA6IHN0cmluZykgOiB2b2lkXG50eXBlIFQgPSAodGhpcyA6IHN0cmluZykgPT4gdm9pZFxuZnVuY3Rpb24geiAodGhpcyA6IHN0cmluZykge31cbmZ1bmN0aW9uIHUgKHRoaXMgOiBzdHJpbmcsIC4uLmEpIHt9XG5mdW5jdGlvbiB2ICh0aGlzIDogc3RyaW5nXG4gICAsIC4uLmEpIHt9XG5mdW5jdGlvbiB3ICh0aGlzXG4gIDogc3RyaW5nXG5cbiAgICwpIHt9XG5mdW5jdGlvbiB4ICh0aGlzXG4gIDogc3RyaW5nXG5cbiAgICxcbiAgIC4uLmEpIHt9XG5mdW5jdGlvbiBpKFxuICB0aGlzOiBYLFxuKSB7fVxuZnVuY3Rpb24gaihcbiAgdGhpczogWCxcbiAgYTogc3RyaW5nXG4pIHt9XG5cbmZ1bmN0aW9uIGpqKFxuICB0aGlzOiBYLFxuICBhOiBzdHJpbmdcbikge1xuICBmdW5jdGlvbiBqamoodGhpczogWCwgYTogc3RyaW5nKSB7fVxufVxuXG5jb25zdCBmID0gZnVuY3Rpb24odGhpczogc3RyaW5nKSB7fVxuY29uc3QgZyA9IGZ1bmN0aW9uKHRoaXM6IHN0cmluZywgLi4uYSkge31cbmNvbnN0IGggPSBmdW5jdGlvbih0aGlzXG46IHN0cmluZyxcbi4uLmEpIHt9XG5jb25zdCBrID0gZnVuY3Rpb24odGhpc1xuOiBzdHJpbmdcblxuLCkge31cbmNvbnN0IGtrID0gZnVuY3Rpb24odGhpc1xuOiBzdHJpbmcsXG5hOiBzdHJpbmcsKSB7fVxuXG4vLyBgYXNgIGNhc3RcbmNvbnN0IGFzQW55ID0gJ2FueScgYXMgYW55O1xuY29uc3QgYXNBcnJheSA9IFsxLCAyLCAzXSBhcyBudW1iZXJbXTtcbmNvbnN0IGFzQmlnSW50TGl0ZXJhbCA9IDFuIGFzIDFuO1xuY29uc3QgYXNCaWdJbnQgPSAxbiBhcyBiaWdpbnQ7XG5jb25zdCBhc0Jvb2xlYW5MaXRlcmFsID0gdHJ1ZSBhcyB0cnVlO1xuY29uc3QgYXNCb29sZWFuID0gdHJ1ZSBhcyBib29sZWFuO1xuY29uc3QgYXNDb21wb25lbnQgPSAoKCkgPT4ge30pIGFzIGNvbXBvbmVudChwOiBudW1iZXIsIG8/OiBzdHJpbmcpO1xuY29uc3QgYXNFbXB0eSA9IHt9IGFzIGVtcHR5O1xuY29uc3QgYXNFeGlzdHMgPSAnZXhpc3RzJyBhcyAqO1xuY29uc3QgYXNGdW5jdGlvbiA9ICgoKSA9PiB7fSkgYXMgKCkgPT4gdm9pZDtcbmNvbnN0IGFzR2VuZXJpYyA9ICdnZW5lcmljJyBhcyAkTm9uTWF5YmVUeXBlPHN0cmluZz47XG5jb25zdCBhc0tleW9mID0gJ2EnIGFzIGtleW9mIHsgYTogc3RyaW5nOyBiOiBudW1iZXIgfTtcbmNvbnN0IGFzTWl4ZWQgPSAnbWl4ZWQnIGFzIG1peGVkO1xuY29uc3QgYXNOdWxsYWJsZSA9IG51bGwgYXMgP3N0cmluZztcbmNvbnN0IGFzTnVsbExpdGVyYWwgPSBudWxsIGFzIG51bGw7XG5jb25zdCBhc051bWJlckxpdGVyYWwgPSAxIGFzIDE7XG5jb25zdCBhc051bWJlciA9IDEgYXMgbnVtYmVyO1xuY29uc3QgYXNPYmplY3QgPSB7IGE6ICdhJyB9IGFzIHsgYTogc3RyaW5nIH07XG5jb25zdCBhc1N0cmluZ0xpdGVyYWwgPSAnbGl0ZXJhbCcgYXMgJ2xpdGVyYWwnO1xuY29uc3QgYXNTdHJpbmcgPSAnc3RyaW5nJyBhcyBzdHJpbmc7XG5jb25zdCBhc1N5bWJvbCA9IFN5bWJvbCgnc3ltYm9sJykgYXMgc3ltYm9sO1xuY29uc3QgYXNUdXBsZSA9IFsnYScsIDFdIGFzIFtzdHJpbmcsIG51bWJlcl07XG5jb25zdCBhc1R5cGVvZiA9ICd0eXBlb2YnIGFzIHR5cGVvZiBhc1N0cmluZztcbmNvbnN0IGFzVW5pb24gPSAndW5pb24nIGFzIHN0cmluZyB8IG51bWJlcjtcbmNvbnN0IGFzVm9pZCA9IHVuZGVmaW5lZCBhcyB2b2lkO1xuXG50eXBlIENvbmRpdGlvbmFsVHlwZTxUPiA9IFQgZXh0ZW5kcyBzdHJpbmcgPyBzdHJpbmcgOiBudW1iZXI7XG5jb25zdCBhc0NvbmRpdGlvbmFsID0gJ2NvbmRpdGlvbmFsJyBhcyBDb25kaXRpb25hbFR5cGU8c3RyaW5nPjtcblxuaW50ZXJmYWNlIEludGVyZmFjZVR5cGUgeyBhOiBzdHJpbmc7IGI6IG51bWJlcjsgfVxuY29uc3QgYXNJbnRlcmZhY2UgPSB7IGE6ICdhJywgYjogMSB9IGFzIEludGVyZmFjZVR5cGU7XG5cbnR5cGUgSW5mZXJUeXBlPFQ+ID0gVCBleHRlbmRzIGluZmVyIFUgPyBVIDogbmV2ZXI7XG5jb25zdCBhc0luZmVyID0gJ2luZmVyJyBhcyBJbmZlclR5cGU8c3RyaW5nPjtcblxudHlwZSBJbnRlcnNlY3Rpb25UeXBlID0geyBhOiBzdHJpbmcgfSAmIHsgYjogbnVtYmVyIH07XG5jb25zdCBhc0ludGVyc2VjdGlvbiA9IHsgYTogJ2EnLCBiOiAxIH0gYXMgSW50ZXJzZWN0aW9uVHlwZTtcblxuY29uc3QgYXNJbmRleGVkID0gJ2luZGV4ZWQnIGFzIFtzdHJpbmcsIG51bWJlcl1bMF07XG5cbi8vIGBhcyBjb25zdGBcbidzJyBhcyBjb25zdDtcblsncyddIGFzIGNvbnN0O1xuIl19
