// @flow

type F = (x: string) => void;

function x0(f: F) {
  f(/* here */)
}

function x1(f: F | void) {
  f?.(/* here */)
}

function x2(f: ?{ m: F }) {
  f?.m(/* here */)
}

function x3(f: ?{ [key: string]: F }) {
  f?.["abc"](/* here */)
}

function x4(f: ?{ m: { n: F } }) {
  f?.m.n(/* here */)
}

function x5(f: { m: ?F }) {
  f.m?.(/* here */)
}

function x6(g: ?{ f: F }) {
  if (g != null) {
    g.f(/* here */)
  }
}

function jsdoc() {
  /**
   * @param x - 1st param
   * @param y - 2nd param
   */
  declare function foo(x: 1, y: 2): 1;

  foo(/* here */);
  foo(1, /* here */);
}
