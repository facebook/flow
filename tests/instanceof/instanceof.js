/* @flow */

// x instancof t
class X1 { foo: number; };
class X2 { foo: string; };

function x(b: boolean) { return b ? new X1 : new X2; }

function consumer1(b: boolean) {
    var g = x(b);
    if (g instanceof X2) g.foo = '1337';
    else g.foo = 1337;
}

function consumer2(b: boolean) {
    var g = x(b);
    if (g instanceof X1) g.foo = '1337';  // oops
}

// x.y instanceof t
class Y1 { bar: X1; };
class Y2 { bar: X2; };

function y(b: boolean) { return b ? new Y1 : new Y2; }

function consumer3(b: boolean) {
    var g = y(b);
    if (g.bar instanceof X2) g.bar.foo = '1337';
    else g.bar.foo = 1337;
}

function consumer4(b: boolean) {
    var g = y(b);
    if (g.bar instanceof X1) g.bar.foo = '1337';  // oops
}

// x.y.z instance of t
class Z1 { baz: Y1; };
class Z2 { baz: Y2; };

function z(b: boolean) { return b ? new Z1 : new Z2; }

function consumer5(b: boolean) {
    var g = z(b);
    if (g.baz.bar instanceof X2) g.baz.bar.foo = '1337';
    else g.baz.bar.foo = 1337;
}

function consumer6(b: boolean) {
    var g = z(b);
    if (g.baz.bar instanceof X1) g.baz.bar.foo = '1337';  // oops
}

// this instanceof t
class C {
  m() {
    if (this instanceof D)
      alert(this.s);
    else
      alert("nope");
  }
}

class D extends C {
  s: string;
  constructor() {
    super();
    this.s = "yup";
  }
}


function foo0(x: Array<number> | number) {
  if (x instanceof Array) {
    x[0] = 123;
  } else {
    x++;
  }
}

function foo1(x: Array<number> | number) {
  if (x instanceof Array) {
    x++; // error
  } else {
    x[0] = 123; // error
  }
}

function nonObjectRHS(x: Object) {
  const y = x instanceof 'bad'; // error
  if (x instanceof 'bad') {x;} // error
  if (x instanceof ('bad': any)) {x;} // ok
  if (x instanceof ('bad': mixed)) {x;} // error
}

function not_refinement_or_val_rhs(x: Object) {
  const immutable = {Map: class Map {}}
  if (x instanceof immutable.Map) {(x: immutable.Map)}
}


function class_explicit() {
  declare var x: mixed;

  class B {}

  var A = { B };

  if (x instanceof A.B) {
    (x: empty); //error
    (x: B);
  }
}


function class_util() {
  declare var x: mixed;

  class B {}

  declare var A: { B: Class<B>};

  if (x instanceof A.B) {
    (x: empty); //error
    (x: B);
  }
}

function class_util_chain() {
  declare var x: mixed;

  class B {}

  declare var A: ?{ B: Class<B>};

  if (x instanceof A?.B) { //error
    (x: empty); //error
    (x: B);
  }
}
