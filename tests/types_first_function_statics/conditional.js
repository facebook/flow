// @flow

declare var b: boolean;
declare var n: number;

// Function declaration
export function f() {}

f.legal = 1;

if (b) {
  f.a = 1;
}

switch (n) {
  case 0:
    f.b = 1;
    break;
}

// Arrow function
const g = () => {};

g.legal = 1;

if (b) {
  g.a = 1;
}

export {g};
