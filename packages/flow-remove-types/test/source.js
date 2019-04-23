/* @flow */

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
