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

// Enums
enum MyStringEnum {
  One,
  Two,
  Three,
}

enum MyNumberEnum of number {
  A = 1,
  B = 2,
  C = 3,
}

enum MyBooleanEnum of boolean {
  Yes = true,
  No = false,
}

const enumValue: MyStringEnum = MyStringEnum.One;

// Large enum with 100 members
enum LargeEnum {
  Member001,
  Member002,
  Member003,
  Member004,
  Member005,
  Member006,
  Member007,
  Member008,
  Member009,
  Member010,
  Member011,
  Member012,
  Member013,
  Member014,
  Member015,
  Member016,
  Member017,
  Member018,
  Member019,
  Member020,
  Member021,
  Member022,
  Member023,
  Member024,
  Member025,
  Member026,
  Member027,
  Member028,
  Member029,
  Member030,
  Member031,
  Member032,
  Member033,
  Member034,
  Member035,
  Member036,
  Member037,
  Member038,
  Member039,
  Member040,
  Member041,
  Member042,
  Member043,
  Member044,
  Member045,
  Member046,
  Member047,
  Member048,
  Member049,
  Member050,
  Member051,
  Member052,
  Member053,
  Member054,
  Member055,
  Member056,
  Member057,
  Member058,
  Member059,
  Member060,
  Member061,
  Member062,
  Member063,
  Member064,
  Member065,
  Member066,
  Member067,
  Member068,
  Member069,
  Member070,
  Member071,
  Member072,
  Member073,
  Member074,
  Member075,
  Member076,
  Member077,
  Member078,
  Member079,
  Member080,
  Member081,
  Member082,
  Member083,
  Member084,
  Member085,
  Member086,
  Member087,
  Member088,
  Member089,
  Member090,
  Member091,
  Member092,
  Member093,
  Member094,
  Member095,
  Member096,
  Member097,
  Member098,
  Member099,
  Member100,
}
