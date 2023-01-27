type T = 3 | 4;
type U = "A" | "B";

function foo1 (o1 : T, o2 : U) {
  if (o1 == o2) { // error

  }
}

type A = "A" | 1 | 2;
type B = "B" | 3 | "C";

function foo2 (o1 : A, o2 : B) {
  if (o1 == o2) { // error

  }
}

declare var cond : boolean;
const i = cond ? 1 : -1;
if (i >= 0) {} // ok

declare var x1: number | string;
declare var y1: number | string;
x1 != y1; // error

const x2 = (1: ?number) || "";
const y2 = (1: ?number) || "";
x2 != y2; // errors
