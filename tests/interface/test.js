/* @flow */

type AI = class {
  /*
   * TODO: When absent, does the interface impose the default constructor of the
   * ES6 spec, or does the ctor remain unbound?  I think that it should remain
   * unbound.
   */
//  constructor(): void;
  method1(): number;
  static sMethod1(): string;
//  static sMethodK(): number;
}

function m1(a: AI): number {
  return a.method1();
}

function sm1(a: AI): string {
  return a.constructor.sMethodAsdf();
  /*
   * This passes?! It should map constructor to the class--I think that this is
   * related to my unmerged PR.
   */
  /*
   * `class` prefix on the type declaration clarifies that `constructor` exists
   * (ES6 default or spec'ed above).  For an object, the Object constructor
   * exists, so (1)in the absence of an explicit constructor spec or (2)in
   * the presence of a constructor that is compatible with that of Object, an
   * object may satisfy the interface.  The `class` prefix is warranted for
   * self referencing like ES6 classes admit, e.g. type T = class A {...} =>
   * method signatures on type T can reference interface A.
   */
}

function cs1(K: Class<Ap>): string {
  return K.sMethod1();
}

function cn1(A: Class<AI>): number {
  var a = new A();
  return a.method1();
}

class A {
  method1(): number {
    return 5;
  }
  static sMethod1(): string {
    return "a string";
  }
  static unnecMethod(): number {
    return 5;
  }
}

class Ap extends A {
  unnecMethod(): string {
    return "boring";
  }
}

class B {
  method1(): string {
    return "no good";
  }
  static sMethod1(): number {
    return 7;
  }
}

var a = new A();
var n1: number = m1(a);
var s1: string = sm1(a);
var s2: string = cs1(A);
/*
var n2: number = cn1(A);
var b = new B();
n1 = m1(b);
s1 = sm1(b);
s2 = cs1(B);
n2 = cn1(B);
*/
