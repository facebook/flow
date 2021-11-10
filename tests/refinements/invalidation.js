// @flow
// Test that an invalidated refinement stays invalidated.
// In particular issues #8251, #8777, #8778, and #8779.

// First, write `coerce`, to demonstrate each issue in turn.

const v: { f?: mixed } = {};
const vv = v;
const store = (x: mixed) => { v.f = () => x; };

function coerce8251<A, B>(x: A): B {
  v.f = () => { throw null };  // Refine v.f.
  if (true) store(x);  // Invalidate the refinement.
  return v.f(); // TODO error: trying to still use the refinement.
}

function coerce8777<A, B>(x: A): B {
  v.f = () => { throw null };  // Refine v.f.
  if (true) vv.f = () => x;  // Invalidate the refinement.
  return v.f(); // TODO error: trying to still use the refinement.
}

function coerce8778<A, B>(x: A): B {
  v.f = () => { throw null };  // Refine v.f.
  for (const i of [0]) store(x);  // Invalidate the refinement.
  return v.f(); // TODO error: trying to still use the refinement.
}

function coerce8779<A, B>(x: A): B {
  v.f = () => { throw null };  // Refine v.f.
  for (let i = 0; i < 1; i++) store(x);  // Invalidate the refinement.
  return v.f(); // TODO error: trying to still use the refinement.
}


// Then, systematically explore different cases.

type A = {b?: {c: boolean}};

// Heap refinements, on `a.b` where `a` is local,
// invalidated by a function call.
(a: A) => {
  // Here's an error, a refinement that fixes it,
  // and a function call correctly invalidating the refinement.
  if (1)                                      a.b.c;   // error
  if (a.b)                                    a.b.c;   // ok
  if (a.b) { f();                             a.b.c; } // error

  // The refinement should stay invalidated anywhere that the control
  // flow could reach after the function call (without passing through
  // somewhere it gets refined again.)

  // Conditional constructs: the short-circuiting operators…
  if (a.b) { true && f();                     a.b.c; } // TODO error
  if (a.b) { f() && true;                     a.b.c; } // error
  if (a.b) { false || f();                    a.b.c; } // TODO error
  if (a.b) { f() || false;                    a.b.c; } // error
  if (a.b) { null ?? f();                     a.b.c; } // error
  if (a.b) { f() ?? true;                     a.b.c; } // error
  // … the ternary operator…
  if (a.b) { p ? f() : 0;                     a.b.c; } // TODO error
  if (a.b) { p ? 1 : f();                     a.b.c; } // TODO error
  if (a.b) { f() ? 1 : 0;                     a.b.c; } // error
  // … and if/else.
  if (a.b) { if (p) f();                      a.b.c; } // TODO error
  if (a.b) { if (p); else f();                a.b.c; } // TODO error
  if (a.b) { if (f());                        a.b.c; } // error

  // Switch statements.
  if (a.b) { switch (p) { case true: f(); }        a.b.c; } // TODO error
  if (a.b) { switch (p) { case true: f(); break; } a.b.c; } // error
  if (a.b) { switch (p) { default: f(); }          a.b.c; } // error
  if (a.b) { switch (p) { default: f(); break; }   a.b.c; } // error
  if (a.b) { switch (f()) {}                       a.b.c; } // error
  if (a.b) { switch (f()) { case true: }           a.b.c; } // error
  if (a.b) { switch (f()) { case true: break; }    a.b.c; } // error
  if (a.b) { switch (f()) { default: }             a.b.c; } // error
  if (a.b) { switch (f()) { default: break; }      a.b.c; } // error

  // Loop constructs.
  let i;
  if (a.b) { while (p) f();                   a.b.c; } // TODO error
  if (a.b) { while (f());                     a.b.c; } // error
  if (a.b) { do f(); while (p);               a.b.c; } // error
  if (a.b) { do; while (f());                 a.b.c; } // error
  if (a.b) { for (; p;) f();                  a.b.c; } // TODO error
  if (a.b) { for (; p; f());                  a.b.c; } // TODO error
  if (a.b) { for (; f(););                    a.b.c; } // error
  if (a.b) { for (f(); p;);                   a.b.c; } // error
  if (a.b) { for (i in y) f();                a.b.c; } // TODO error
  if (a.b) { for (i in ff());                 a.b.c; } // TODO error
  if (a.b) { for (i of y) f();                a.b.c; } // TODO error
  if (a.b) { for (i of ff());                 a.b.c; } // TODO error

  // Loop constructs with an abrupt completion / abnormal control flow.
  // while…
  if (a.b) { while (p) { f(); continue; }     a.b.c; } // TODO error
  if (a.b) { while (p) { f(); break;    }     a.b.c; } // TODO error
  if (a.b) { while (p) { f(); return;   }     a.b.c; } // ideally ok, error acceptable
  if (a.b) { while (f()) { continue; }        a.b.c; } // error
  if (a.b) { while (f()) { break;    }        a.b.c; } // error
  if (a.b) { while (f()) { return;   }        a.b.c; } // error
  // … do-while…
  if (a.b) { do { f(); continue; } while (p); a.b.c; } // TODO error
  if (a.b) { do { f(); break;    } while (p); a.b.c; } // TODO error
  if (a.b) { do { f(); return;   } while (p); a.b.c; } // error[unreachable-code]
  if (a.b) { do { continue; } while (f());    a.b.c; } // TODO error
  if (a.b) { do { break;    } while (f());    a.b.c; } // ideally ok, error acceptable
  if (a.b) { do { return;   } while (f());    a.b.c; } // error[unreachable-code]
  // … for…
  if (a.b) { for (; p;) { f(); continue; }    a.b.c; } // TODO error
  if (a.b) { for (; p;) { f(); break;    }    a.b.c; } // TODO error
  if (a.b) { for (; p;) { f(); return;   }    a.b.c; } // ideally ok, error acceptable
  if (a.b) { for (; p; f()) continue;         a.b.c; } // TODO error
  if (a.b) { for (; p; f()) break;            a.b.c; } // ideally ok, error acceptable
  if (a.b) { for (; p; f()) return;           a.b.c; } // ideally ok, error acceptable
  if (a.b) { for (; f();) continue;           a.b.c; } // error
  if (a.b) { for (; f();) break;              a.b.c; } // error
  if (a.b) { for (; f();) return;             a.b.c; } // error
  if (a.b) { for (f(); p;) continue;          a.b.c; } // error
  if (a.b) { for (f(); p;) break;             a.b.c; } // error
  if (a.b) { for (f(); p;) return;            a.b.c; } // error
  // … for-in…
  if (a.b) { for (i in y) { f(); continue; }  a.b.c; } // TODO error
  if (a.b) { for (i in y) { f(); break;    }  a.b.c; } // TODO error
  if (a.b) { for (i in y) { f(); return;   }  a.b.c; } // ideally ok, error acceptable
  if (a.b) { for (i in ff()) continue;        a.b.c; } // TODO error
  if (a.b) { for (i in ff()) break;           a.b.c; } // TODO error
  if (a.b) { for (i in ff()) return;          a.b.c; } // TODO error
  // … for-of.
  if (a.b) { for (i of y) { f(); continue; }  a.b.c; } // TODO error
  if (a.b) { for (i of y) { f(); break;    }  a.b.c; } // TODO error
  if (a.b) { for (i of y) { f(); return;   }  a.b.c; } // ideally ok, error acceptable
  if (a.b) { for (i of ff()) continue;        a.b.c; } // TODO error
  if (a.b) { for (i of ff()) break;           a.b.c; } // TODO error
  if (a.b) { for (i of ff()) return;          a.b.c; } // TODO error
}

// Heap refinements on `a.b`, cont'd.
// (As a separate function just to avoid a perf issue.)
(a: A) => {
  let i;

  // Labelled break, to labelled statements that have no "break"
  // semantics of their own (i.e. not loops or `switch`.)
  if (a.b) { l: { f(); }                           a.b.c; } // error
  if (a.b) { l: { f(); break l; }                  a.b.c; } // TODO error
  if (a.b) { l: if (p) { f(); break l; }           a.b.c; } // TODO error
  l: if (a.b) { if (p) { f(); break l; }           a.b.c; } // ideally ok, error acceptable
  l: if (a.b) { f(); break l;                      a.b.c; } // error[unreachable-code]

  // Labelled break to `switch`.
  if (a.b) { l: switch (p) { case true: f(); break l; } a.b.c; } // TODO error
  if (a.b) { l: switch (p) { default: f(); break l; }   a.b.c; } // TODO error
  if (a.b) { l: switch (f()) { case true: break l; }    a.b.c; } // error
  if (a.b) { l: switch (f()) { default: break l; }      a.b.c; } // error

  // Labelled break and continue, to loop constructs.
  // while…
  if (a.b) { l: while (p) { f(); continue l; }     a.b.c; } // TODO error
  if (a.b) { l: while (p) { f(); break l;    }     a.b.c; } // TODO error
  if (a.b) { l: while (f()) { continue l; }        a.b.c; } // error
  if (a.b) { l: while (f()) { break l;    }        a.b.c; } // error
  // … do-while…
  if (a.b) { l: do { f(); continue l; } while (p); a.b.c; } // TODO error
  if (a.b) { l: do { f(); break l;    } while (p); a.b.c; } // TODO error
  if (a.b) { l: do { continue l; } while (f());    a.b.c; } // TODO error
  if (a.b) { l: do { break l;    } while (f());    a.b.c; } // ideally ok, error acceptable
  // … for…
  if (a.b) { l: for (; p;) { f(); continue l; }    a.b.c; } // TODO error
  if (a.b) { l: for (; p;) { f(); break l;    }    a.b.c; } // TODO error
  if (a.b) { l: for (; p; f()) continue l;         a.b.c; } // TODO error
  if (a.b) { l: for (; p; f()) break l;            a.b.c; } // ideally ok, error acceptable
  if (a.b) { l: for (; f();) continue l;           a.b.c; } // error
  if (a.b) { l: for (; f();) break l;              a.b.c; } // error
  if (a.b) { l: for (f(); p;) continue l;          a.b.c; } // error
  if (a.b) { l: for (f(); p;) break l;             a.b.c; } // error
  // … for-in…
  if (a.b) { l: for (i in y) { f(); continue l; }  a.b.c; } // TODO error
  if (a.b) { l: for (i in y) { f(); break l;    }  a.b.c; } // TODO error
  if (a.b) { l: for (i in ff()) continue l;        a.b.c; } // TODO error
  if (a.b) { l: for (i in ff()) break l;           a.b.c; } // TODO error
  // … for-of.
  if (a.b) { l: for (i of y) { f(); continue l; }  a.b.c; } // TODO error
  if (a.b) { l: for (i of y) { f(); break l;    }  a.b.c; } // TODO error
  if (a.b) { l: for (i of ff()) continue l;        a.b.c; } // TODO error
  if (a.b) { l: for (i of ff()) break l;           a.b.c; } // TODO error
}

// Heap refinements, again on `a.b` where `a` is local,
// invalidated by a write to property `b` on some other heap object.
declare var x: { b?: { ... } };
(a: A) => {
  // Here's an error, a refinement that fixes it,
  // and a write correctly invalidating the refinement.
  if (a.b)                                    a.b.c;   // ok
  if (a.b) { delete x.b;                      a.b.c; } // error
  if (a.b) { x.b = y;                         a.b.c; } // error

  // The refinement should stay invalidated anywhere that the control
  // flow could reach after the write (without passing through
  // somewhere it gets refined again.)

  // Conditional constructs: the short-circuiting operators…
  if (a.b) { true && delete x.b;              a.b.c; } // TODO error
  if (a.b) { delete x.b && true;              a.b.c; } // error
  if (a.b) { true && (x.b = y);               a.b.c; } // TODO error
  if (a.b) { (x.b = y) && true;               a.b.c; } // error
  // (From here, we forget `delete` and just use assignment.)
  if (a.b) { false || (x.b = y);              a.b.c; } // TODO error
  if (a.b) { (x.b = y) || false;              a.b.c; } // error
  if (a.b) { null ?? (x.b = y);               a.b.c; } // error
  if (a.b) { (x.b = y) ?? true;               a.b.c; } // error
  // … the ternary operator…
  if (a.b) { p ? (x.b = y) : 0;               a.b.c; } // TODO error
  if (a.b) { p ? 1 : (x.b = y);               a.b.c; } // TODO error
  if (a.b) { (x.b = y) ? 1 : 0;               a.b.c; } // error
  // … and if/else.
  if (a.b) { if (p) x.b = y;                  a.b.c; } // TODO error
  if (a.b) { if (p); else x.b = y;            a.b.c; } // TODO error
  if (a.b) { if (x.b = y);                    a.b.c; } // error

  // Switch statements.
  if (a.b) { switch (p) { case true: x.b = y; }        a.b.c; } // TODO error
  if (a.b) { switch (p) { case true: x.b = y; break; } a.b.c; } // error
  if (a.b) { switch (p) { default: x.b = y; }          a.b.c; } // error
  if (a.b) { switch (p) { default: x.b = y; break; }   a.b.c; } // error
  if (a.b) { switch (x.b = y) {}                       a.b.c; } // error
  if (a.b) { switch (x.b = y) { case true: }           a.b.c; } // error
  if (a.b) { switch (x.b = y) { case true: break; }    a.b.c; } // error
  if (a.b) { switch (x.b = y) { default: }             a.b.c; } // error
  if (a.b) { switch (x.b = y) { default: break; }      a.b.c; } // error

  // Loop constructs.
  let i;
  if (a.b) { while (p) x.b = y;               a.b.c; } // TODO error
  if (a.b) { while (x.b = y);                 a.b.c; } // error
  if (a.b) { do x.b = y; while (p);           a.b.c; } // error
  if (a.b) { do; while (x.b = y);             a.b.c; } // error
  if (a.b) { for (; p;) x.b = y;              a.b.c; } // TODO error
  if (a.b) { for (; p; x.b = y);              a.b.c; } // TODO error
  if (a.b) { for (; x.b = y;);                a.b.c; } // error
  if (a.b) { for (x.b = y; p;);               a.b.c; } // error
  if (a.b) { for (i in y) x.b = y;            a.b.c; } // TODO error
  if (a.b) { for (i in x.b = y);              a.b.c; } // TODO error
  if (a.b) { for (i of y) x.b = y;            a.b.c; } // TODO error
  if (a.b) { for (i of x.b = y);              a.b.c; } // TODO error

  // We'll skip running through the other variations in this version:
  // continue, break, return, and labelled break and continue.
}

declare var f: mixed => boolean;
declare var p: boolean;

declare var y: { ... } & Iterator<mixed>;
declare var ff: mixed => { ... } & Iterator<mixed>;
