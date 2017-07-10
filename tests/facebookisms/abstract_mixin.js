/* @flow */

var mixin = require('mixin');

class A {
  abstract m(): void;
  abstract static static_m(): void;
}

class B extends mixin(A) {} //ng
class C extends mixin(A) {
  m() {}
  static static_m() {}
} //ng
