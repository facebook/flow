
function A(a: null | number) {//Using TS syntax to be able to compare with TS
  a! as number;
  a! as empty; // LHS is number
}

function B(a: number) {
  a! as number;
  a! as empty; // LHS is number
}

function C(f: () => number | null) {
  f()! as number;
  f()! as empty; // LHS is number
  f!() as number // LHS is ?number
}

function D(g: null | (() => number)){
  g!() as number;
  g!() as empty; // LHS is number
  g()! // Calling null
}

function E(a: null | { b: () => null | { c: null | number }}) {
  a!.b()!.c! as number;
  a!.b()!.c! as empty; // LHS is number
}

function F(a: null | { b: null | { c: number }}) {
  a?.b!.c as number; // LHS is ?number
  a?.b!.c! as number;
  a?.b!.c! as empty; // LHS is number
  (a?.b)!.c as number;
  (a?.b)!.c as empty; //LHS is number
}

function G(a: null | { b: () => null | { c: null | number }}) {
  a!.b()!.c! as number;
  a!.b()!.c! as empty; // LHS is number
  a!.b()!['c'] as number;
  a!.b()!['c'] as empty; // LHS is number
  a?.b()!['c'] as number; // LHS is ?number
  a?.b()!['c']! as number;
  a?.b()!['c']! as empty; // LHS is number
}


function H(a: null | { b: null | () => null | { c: null | number }}) {
  a!.b!()!.c! as number;
  a!.b!()!.c! as empty; // LHS is number
}

function I(a: null | { b: () => null | { c: number }}) {
  a?.b()!['c'] as number; // LHS is ?number
  (a?.b())!['c'] as number;
  (a?.b())!['c'] as empty; // LHS is number
}

function J(a: null | () => { b: null | { c: number }}) {
  a?.().b.c as number // LHS is ?number from both nulls
  a?.().b!.c as number // LHS is ?number from only a's null
  (a?.().b)!.c as number //
  (a?.().b)!.c as empty // LHS is number
}

class K {
  #p: number = 42;

  m(c: null | K) {
    c!.#p as number;
    c!.#p as empty;
  }
}
