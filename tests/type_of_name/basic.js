// @flow

// Basic types
const myString: string = "hello world";
const myNumber: number = 42;
const myBoolean: boolean = true;
const myArray: Array<number> = [1, 2, 3];
const myObject: {x: number, y: string} = {x: 10, y: "test"};

// Functions
function myFunction(param: boolean): string {
  return param ? "true" : "false";
}

const arrowFunc = (input: number): number => input * 2;

// Classes
class MyClass {
  propertyName: string;

  constructor(value: string) {
    this.propertyName = value;
  }

  methodName(): number {
    return 123;
  }
}

const instance = new MyClass("test");

// Type aliases
type MyType = {foo: string, bar: number};
const typeVar: MyType = {foo: "hello", bar: 42};

// Destructuring
const {x: destructuredX, y: destructuredY} = myObject;

// Let and var
let letVariable = 100;
var varVariable = "variable";

// Optional types
const optionalString: ?string = null;

// Union types
const unionVar: string | number = "could be either";

// Function types
const functionType: (x: number) => string = (x) => x.toString();

// Export
export const exportedVar = "exported";
export {myString, MyClass};
