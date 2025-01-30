// This is a regression test against a bug that required keeping track of a list
// of indices in LatentP.
function test3() {
  declare var refinesFst: (a: mixed, b: mixed) => a is number;
  declare var refinesSnd: (a: mixed, b: mixed) => b is number;

  function fn1(x: number | string, y: number | string) {
    if (refinesFst(x, x)) {
      x as number; // okay
      return;
    }
    x as string; // okay
  }

  function fn2(x: number | string, y: number | string) {
    if (refinesSnd(x, x)) {
      x as number; // okay
      return;
    }
    x as string; // okay
  }
}
