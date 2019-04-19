// @flow

class C1 {
  constructor(): T {} // ok
}

class C2 {
  constructor: T = e; // error
}
