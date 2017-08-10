//@flow
class A {
  #p: number
  constructor() {this.#p = 3}
}

class C {
  #p: number
  #q: number
  constructor() {
    this.#p = 0; // in scope
    this.#q; // in scope
  }
  test(other: C) {
    other.#q; // in lexical scope, object has the property.
  }
}

class D {
  #p: number // Note that C also has a #p with number, and #p is in
             // lexical scope
  constructor() {this.#p = 3}
  test(other: C) {
    other.#p; // Error: other does not have access to private variables in here.
  }
}

class Outer {
 #outer: number
 constructor() {
   var Inner = class A {
      #inner: Outer
      constructor() {
        this.#inner = new Outer();
        var x = new Outer();
        x.#outer = 0; // Both inner and outer should be visible here!
        x.#inner = new Outer(); // Error, #inner is not a private field of Outer
      }
   };
 }
}

class Clash {
  #p: number
  constructor() {
    var Inner = class A {
      #p: string
      constructor() {
        (this.#p : number); // Error, #p is a string not a number
        this.#p = "test";
        this.#p = 3; // Error, #p is a string not a number
        var x = new Clash();
        (x.#p : string) // Error, here #p is a number not a string
      }
    }
  }
}

class Refinements {
  #p: ?number
  constructor () {
    if (this.#p) {
      (this.#p : number); // Refinements work properly
    } else {
      (this.#p : number); // Error #p is null or undefined
    }
  }
}

class IncompatibleGetAndSet {
  #p: number
  static #q: number
  constructor () {
    const a: number = 3;
    a.#p = 3; // Error, #p not on number
    const b: {p: number} = {p: 3}
    b.#p; // Error, #p not on b.
  }
}

class Static {
  static #p: number
  static #q: number
  #r: number
  m(): number {
    Static.#p = 2;
    if (Static.#p === 3) { // Refinements still work
      (Static.#p : 3);
    }
    return Static.#p;
  }
 bad() {
   IncompatibleGetAndSet.#q; // Error, not visible here
   Static.#q = Static.#p;
   this.#p = 3; // Error, no #p on instance
   this.#q; // Error, no #q on instance
   Static.#r; // Error, no #r on class
   Static.#r = Static.#q; // Error, no #r on class
 }
}
