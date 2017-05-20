/* @noflow */

interface IFoo {
  foo: string;
  static bar: number;
}

class C1 implements IFoo {} //ng
class C2 implements IFoo {
  foo: string;
  static bar: string; //ng
}

class Impl1 {
  foo: string;
  static bar: string; //wrong type
}
Impl1.bar = "s1";
let i1: IFoo = new Impl1; //ng (the error message is icky, citing the "wrong type" line)
let I1: Class<IFoo> = Impl1; //ng  (the error message is icky, citing the "wrong type" line)

class Impl2 {
  foo: string;
  static bar: number;
}
let i2: IFoo = new Impl2;
let I2: Class<IFoo> = Impl2;

function F1() {}
F1.foo = "s2";
let f1: IFoo = F1; //ng

function F2() { this.foo = "s3"; }
F1.bar = 3;
let f2: IFoo = new F2;

interface IFlat1 {
  foo: string;
  bar: number;
}

function F3() {}
F3.foo = "s4";
F3.bar = 5;
let f3: IFlat1 = F3;

interface IFlat2 {
  static (): void;
  static foo: string;
  static bar: number;
}

function F4() {}
F4.foo = "s6";
F4.bar = 7;
let flat2: Class<IFlat2> = F4;
