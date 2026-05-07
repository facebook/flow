class C { p: string }
declare const c: C;
if (c instanceof Object) { // refinement succeeds
  c.p as empty; // error: string ~> empty
}
