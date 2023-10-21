// @flow

type F = (x: string) => void;

function x1() {
  declare const f: F | void;
  f?.("abc");
}

function x2() {
  declare const f: ?{ m: F };
  f?.m("abc");
}

function x3() {
  declare const f: ?{ [key: string]: F };
  f?.["abc"]("abc");
}

function x4() {
  declare const f: ?{ m: { n: F } };
  f?.m.n("abc");
}

function x5() {
  declare const f: { m: ?F };
  f.m?.("abc");
}

function x6() {
  declare const g: ?{ f: F };
  if (g != null) {
    g.f("abc");
  }
}
