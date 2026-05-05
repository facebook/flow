class C { }
function foo(x: unknown) {
  if (x instanceof C) {
    x as number;
  }
}

class A { }
class B extends A { }
function bar(x: unknown) {
  if (x instanceof B) {
    x as A;
  }
}

class PA<+X> { }
class PB<X> extends PA<X> { }
function baz(x: unknown) {
  if (x instanceof PB) {
    x as PA<any>;
  }
}

function qux_readonlyarray(x: unknown) {
  if (x instanceof Array) {
    x as $ReadOnlyArray<any>;
  }
}

function qux_array(x: unknown) {
  if (x instanceof Array) {
    x as Array<any>;
  }
}

function qux_object(x: unknown) {
  if (x instanceof Object) {
    x.p;
  }
}

function qux_function(x: unknown) {
  if (x instanceof Function) {
    x();
  }
}
