
function A(a: null | number) {//Using TS syntax to be able to compare with TS
  a! as number;
  a! as empty; // LHS is number
}

function B(a: number) {
  a! as number;// ok
  a! as empty; // LHS is number
}

function C(f: () => number | null) {
  f()! as number;// ok
  f()! as empty; // LHS is number
  f!() as number // LHS is ?number
}

function D(g: null | (() => number)){
  g!() as number;// ok
  g!() as empty; // LHS is number
  g()! // Calling null
}

function E(a: null | { b: (() => (null | { c: null | number }))}) {
  a!.b()!.c! as number;// ok
  a!.b()!.c! as empty; // LHS is number
}

function H(a: null | { b: null | () => null | { c: null | number }}) {
  a!.b!()!.c! as number;// ok
  a!.b!()!.c! as empty; // LHS is number
}

class K {
  #p: number = 42;

  m(c: null | K) {
    c!.#p as number; // ok
    c!.#p as empty; // LHS is number
  }
}

function L(x: {a?: { b?: number }, c: { d?: number, e?: number, f?: number }}) {
  x.a! = 1; // error
  x.a! = ({} as {b?: number}); // ok
  x.a!.b = 42; // ok
  x.a!.b = "43"; // error
  x.f.d += 42; // error addition
  (x.c.d! += 42) as number; // ok
  (x.c.d! += 42) as empty; // LHS is number
  x.c.e! += false; // incompatible operation
}

function M(x: null | number) {
  let y = x;
  y += 10; // error
  let z = x;
  z! += 10; // ok
  let w = x;
  w! += false; // error incompatible
}
