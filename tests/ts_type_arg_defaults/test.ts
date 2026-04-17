// All type params have defaults: missing type args should be OK
type Foo<T = number> = [T];
const a: Foo = [1]; // OK, treated as Foo<number>

// Explicit empty type args should also work
const b: Foo<> = [1]; // OK

// Verify the type is actually applied correctly
const i: Foo = ["not a number"]; // ERROR: string ~> number

// No defaults: missing type args should still error
type Bar<T> = [T];
const c: Bar = [1]; // ERROR: missing type args

// Mixed: some params have defaults, some don't
type Baz<T, U = string> = [T, U];
const d: Baz = ["", ""]; // ERROR: T has no default

// All params have defaults
type Qux<T = number, U = string> = [T, U];
const e: Qux = [1, ""]; // OK

// Class with all defaulted type params
class MyClass<T = number> {
  x: T;
  constructor(x: T) {
    this.x = x;
  }
}
const f: MyClass = new MyClass(1); // OK

// Class with required type params
class MyClass2<T> {
  x: T;
  constructor(x: T) {
    this.x = x;
  }
}
const g: MyClass2 = new MyClass2(1); // ERROR: missing type args

// Interface with defaults
interface MyInterface<T = string> {
  x: T;
}
const h: MyInterface = { x: "" }; // OK

// Class extends with all-default type params
class Base<T = number> {
  x: T;
  constructor(x: T) {
    this.x = x;
  }
}
class Child extends Base {} // OK
const j: number = new Child(1).x; // OK, T defaults to number

// Class extends with required type params
class Base2<T> {
  x: T;
  constructor(x: T) {
    this.x = x;
  }
}
class Child2 extends Base2 {} // ERROR: missing type args

// Interface extends with all-default type params
interface IBase<T = string> {
  val: T;
}
interface IChild extends IBase {} // OK
const k: IChild = { val: "" }; // OK, T defaults to string

// Interface extends with required type params
interface IBase2<T> {
  val: T;
}
interface IChild2 extends IBase2 {} // ERROR: missing type args

// Qualified type reference: namespace member with defaults
declare namespace NS {
  type Inner<T = boolean> = { flag: T };
}
const m: NS.Inner = { flag: true }; // OK, T defaults to boolean

// Qualified type reference: namespace member without defaults
declare namespace NS2 {
  type Inner<T> = { flag: T };
}
const n: NS2.Inner = { flag: true }; // ERROR: missing type args
