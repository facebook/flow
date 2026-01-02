// This is a regression test against a bug that required keeping track of a list
// of indices in LatentP.
function test3() {
  declare var refinesFst: (a: unknown, b: unknown) => a is number;
  declare var refinesSnd: (a: unknown, b: unknown) => b is number;

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
