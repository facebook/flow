/* @flow */

class A {
  static static_field: string;
  abstract m(): void;
}
A.static_field = "a string";

class B {
  static static_field: () => string;
  abstract static static_method(): string;
}
B.static_field = function (): string {
  return this.static_method();
};
let s1: string = B.static_field(); //ng

class C extends B {}
let s2: string = C.static_field(); //ng
