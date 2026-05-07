type T = 'A' | 'B';

const E = {A: 'A', B: 'B'} as const;

declare const x: T;

if (x === E.A) {
  E.A as T; // OK
  E.A as number; // Error expected
}

declare const Enum: {
  A: 'A',
  B: 'B',
}

function foo(e: 'A' | 'B') {
    if (e === Enum.A) {
    }
}

declare const Enum2 :{
  A: 1,
  B: 1,
}

function foo2(e: 1 | 2) {
    if (e === Enum2.A) {
    }
}

declare const Enum3 :{
  A: true,
  B: false,
}

function foo3(e: true | false) {
    if (e === Enum3.A) {
    }
}
