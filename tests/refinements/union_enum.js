// @flow


type T = 'A' | 'B';

const E = {A: 'A', B: 'B'};

declare var x: T;

if (x === E.A) {
  (E.A: T); // OK
  (E.A: number); // Error expected
}

declare var Enum: {|
  A: 'A',
  B: 'B',
|}

function foo(e: 'A' | 'B') {
    if (e === Enum.A) {
    }
}

declare var Enum2 :{|
  A: 1,
  B: 1,
|}

function foo2(e: 1 | 2) {
    if (e === Enum2.A) {
    }
}

declare var Enum3 :{|
  A: true,
  B: false,
|}

function foo3(e: true | false) {
    if (e === Enum3.A) {
    }
}
