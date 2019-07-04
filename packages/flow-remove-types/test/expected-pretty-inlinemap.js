/*  */
// @nolint

// Regular import
import {
  Something,
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

  // Class Property

  method() {
    return;
  }
}

// Class expression implements interface
var SomeClass = class Baz {

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

// Comment type includes are emptied out
class MyClass {
  /*:: */
}

// Inferred predicate
function testit(arg) {
  return !!arg;
}

// Test function with default type parameter
function f() {}

//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbInRlc3Qvc291cmNlLmpzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJHQUFHLEFBQUs7Ozs7OztFQU1OLEFBQUEsQUFBSSxDQUFDLEFBQUEsQUFBUSxBQUFDLEFBQUE7RUFDZCxBQUFBLEFBQU0sQ0FBQyxBQUFBLEFBQWMsQUFBQTs7OztBQUl2QixBQUE0QyxBQUFBOzs7cUJBR3ZCLEFBQU0sVUFBVSxBQUFBLEFBQUMsaUJBQWlCLEFBQUEsQUFBQyxPQUFPLEFBQUEsQUFBYyxPQUFPLEFBQVE7O2lCQUUzRSxBQUFLOzs7O0FBSXRCLEFBSUMsQUFBQTs7O0FBR0QsQUFFQyxBQUFBOzs7QUFHRCxBQUVDLEFBQUE7Ozt3QkFHdUIsQUFBQSxBQUFVLE9BQU8sQUFBQSxBQUFHLEFBQUMsQ0FBQyxBQUFBLEFBQVU7O1FBRWhELEFBQVE7OztFQUdkLEFBQUEsQUFBVSxBQUFBOztVQUVGLEFBQU87Ozs7OzswQkFNUyxBQUFBLEFBQVUsQ0FBQyxBQUFBLEFBQUc7RUFDdEMsQUFBQSxBQUFVLEFBQUE7O1VBRUYsQUFBTzs7Ozs7O2FBTUosQUFBRztPQUNULEFBQUc7Ozs7S0FJTCxBQUFHLEVBQUUsQUFBWTs7Ozs7O21DQU1hLEFBQVE7Ozs7O0FBSzNDLEFBRUMsQUFBQTs7O0FBR0QsQUFBa0MsQUFBQTs7O0FBR2xDLEFBRUMsQUFBQTs7O0FBR0QsQUFFQyxBQUFBOzs7QUFHRCxBQUdFLEFBQUE7OztBQUdGLEFBQStCLEFBQUE7OztBQUcvQixBQUFnQixBQUFBOzs7QUFHaEIsQUFBa0IsQUFBQTs7Ozs7O0FBTWxCLEFBQWtDLEFBQUE7Ozs7YUFJckIsQUFBTTs7Ozs7OztBQU9uQixBQUFnRCxBQUFBOztvQkFFNUIsQUFBRyxtQkFBbUIsQUFBQSxBQUFVLENBQUMsQUFBQSxBQUFhOztFQUVoRSxBQUFBLEFBQVEsQUFBQTs7bUJBRVMsQUFBRzs7OztPQUlmLEFBQUc7Ozs7Ozs7NEJBT2tCLEFBQUcsTUFBTSxBQUFHLENBQUMsQUFBWTs7Ozs7QUFLckQsQUFFRyxBQUFBOzs7aUNBRzhCLEFBQWlDOzs7OztzQkFLNUMsQUFDaEI7O3lCQUVtQixBQUFBLEFBQ25COzs7V0FHSyxBQUFRO1lBQ1AsQUFBQSxBQUFNOzs7U0FHVCxBQUFROzs7U0FHUixBQUFTO2dCQUNGLEFBQVM7OztnQkFHVCxBQUFBLEFBQUcsRUFBRSxBQUFHOzs7Ozs7Ozs7Ozs7O09BYWpCLEFBQUEsQUFBYTs7OzttQkFJRCxBQUFPLENBQUMsQUFBUyxDQUFDLEFBQUEsQUFBTzs7Ozs7VUFLbEMsQUFBVSJ9
