// @flow

declare class C1 {
  constructor(): T; // ok
}

declare class C2 {
  constructor: T; // error
}
