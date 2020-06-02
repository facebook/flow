// @flow

declare function invariant(boolean, ?string): empty;
declare var b: boolean;
declare var n: number;

///////////////////////
// Not implicit void //
///////////////////////
function f1(): number {
  return 1;
}
f1();

function f2(): number {
  if (b) {
    return 1;
  } else {
    return 2;
  }
}
f2();

function f3(): number {
  try {
    return 1;
  } catch (e) {
    return 2;
  }
}
f3()

function f4(): number {
  try {
    return 1;
  } catch (e) {
    return 2;
  } finally {
  }
}
f4()

function f5(): number {
  try {
  } catch (e) {
  } finally {
    return 2;
  }
}
f5()

function f6(): number {
  try {
    return 1;
  } finally {
    return 2;
  }
}
f6();

function f7(): number {
  try {
  } finally {
    return 2;
  }
}
f7();

function f8(): number {
  switch (n) {
    case 1:
      return 1;
    default:
      return 2;
  }
}
f8();

function f9(): number {
  switch (n) {
    case 1:
      "foo";
    case 2:
      return 1;
    default:
      return 2;
  }
}
f9();

function f10(): number {
  switch (n) {
    case 1:
      "foo";
      l: if (b) {
        break l;
      }
    case 2:
      return 1;
    default:
      return 2;
  }
}
f10()

function f11(): number {
  switch (n) {
    case 1:
      "foo";
      for (;;) {
        break;
      }
    case 2:
      return 1;
    default:
      return 2;
  }
}
f11()

function f12(): number {
  invariant(false);
}
f12();

function f13(): number {
  invariant(false, "msg");
}
f13();

function f14(): number {
  switch (n) {
    case 1:
      return 1;
    default:
      invariant(false);
  }
}
f14();

function f15(): number {
  try {
    return 1;
  } finally {
  }
}
f15();

function f16(): number {
  switch (n) {
    case 1:
      l: if (b) {
        break l;
      } else {
        break l;
      }
    default:
      return 1;
  }
}
f16();

function f17(): number {
  switch (n) {
    case 1:
      for (let i = 0; i < 2; i++) {
        break;
      }
    default:
      return 1;
  }
}
f17();

function f18(): number {
  while(invariant()) {
  }
}
f18();

function f19(): number {
  const x = invariant();
}
f19();

function f20(): number {
  class C extends invariant() {
  }
}
f20();

function f21(): number {
  if (invariant()) {
  }
}
f21();

function f22(): number {
  switch (invariant()) {
  }
}
f22();

function f23(): number {
  switch (n) {
    case invariant():
  }
}
f23();

///////////////////
// Implicit void //
///////////////////
function g1(): number { // Error
  if (b) {
    return 1;
  }
}
g1();

function g2(): number { // Error
  if (b) {
  } else {
    return 1;
  }
}
g2();

function g3(): number { // Error
  if (b) {
    return 1;
  } else {
  }
}
g3();

function g4(): number { // Error
  try {
    return 1;
  } catch (e) {
  }
}
g4();

function g5(): number { // Error
  try {
    return 1;
  } catch (e) {
  } finally {
  }
}
g5();

function g6(): number { // Error
  switch (n) {
    case 1:
      return 1;
  }
}
g6();

function g7(): number { // Error
  switch (n) {
    case 1:
      "foo";
      break;
    case 2:
      return 1;
    default:
      return 2;
  }
}
g7();

function g8(): number { // Error
  l: switch (n) {
    case 1:
      "foo";
      break l;
    case 2:
      return 1;
    default:
      return 2;
  }
}
g8();

function g9(): number { // Error
  l: for (;;) {
    switch (n) {
      case 1:
        "foo";
        break l;
      case 2:
        return 1;
      default:
        return 2;
    }
  }
}
g9();

function g10(): number { // Error
  switch (n) {
  }
}
g10();

function g11(): number { // Error
  switch (n) {
    default:
  }
}
g11();

function g12(): number { // Error
  switch (n) {
    case 1:
      if (b) {
        break;
      } else {
        break;
      }
    case 2:
      return 1;
    default:
      return 2;
  }
}
g12();

function g13(): number { // Error
  invariant(b);
}
g13();

function g14(): number { // Error
  while (false) {
    invariant();
  }
}
g14();

function g15(): number { // Error
  for (;false;) {
    invariant();
  }
}
g15();

function g16(): number { // Error
  for (const x of []) {
    invariant();
  }
}
g16();

function g17(x: Object): number { // Error
  switch (x.foo) {
    case "bar": break;
    case "baz": break;
  }
}
g17({});
