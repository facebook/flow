// @flow

type Bar1 = {};
import type {Node as Bar2} from 'react';
class Bar3 {}
function Bar4() {}
declare var Bar5: unknown;
component Bar6() { return null; }

class Bar1 { // error - duplicate name
  m() {}
  constructor(foo: string) {
    this.n(); // error, n is not a method
  }
}

class Bar2 { // error - duplicate name
  constructor(foo: string) {}
}

class Bar3 { // error - duplicate name
  constructor(foo: string) {}
}

const bar3 = new Bar3();
bar3.m(); // error, n is not a method in first definition of Bar3

class Bar4 { // error - duplicate name
  constructor(foo: string) {}
}

class Bar5 { // error - duplicate name
  constructor(foo: string) {}
}

class Bar6 { // error - duplicate name
  constructor(foo: string) {}
}
