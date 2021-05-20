// @flow

class A {
  static foo(this : typeof A) {}
  static foo2(this : number) {} // error
  static foo3(this : interface {}) {}
  static foo4(this : A) {} // error
  bar (this : A) {}
  bar2 (this : number) {} // error
  bar3 (this : interface {}) {}
  bar4 (this : typeof A) {} // error
}

declare class B {
  static foo(this : typeof B) : void,
  static foo2(this : number) : void, // error
  static foo3(this : interface {}) : void,
  static foo4(this : B) : void, // error
  bar (this : B) : void,
  bar2 (this : number) : void, // error
  bar3 (this : interface {}) : void,
  bar4 (this : typeof B) : void // error
}

interface C {
  bar (this : C) : void,
  bar2 (this : number) : void, // error
  bar3 (this : interface {}) : void,
}

/*
type D = interface {
  bar (this : D) : void,
  bar2 (this : number) : void, // error
  bar3 (this : {}) : void,
}*/

interface E {
  bar (this : C) : void,
  bar2 (this : number) : void, // error
  bar3 (this : interface {}) : void,
}
