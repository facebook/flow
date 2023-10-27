// @flow

type F = <A>(x: A, y: A, z: Array<A>) => void;

function x0(f: F) {
  f(/* here */)
  f(1,/* here */)
  f(1, "",/* here */)
}

function x1(f: F | void) {
  f?.(/* here */)
  f?.(1,/* here */)
  f?.(1, "",/* here */)
}

function x2(f: ?{ m: F }) {
  f?.m(/* here */)
  f?.m(1,/* here */)
  f?.m(1, "",/* here */)
}

function x3(f: ?{ [key: string]: F }) {
  f?.["abc"](/* here */)
  f?.["abc"](1,/* here */)
  f?.["abc"](1, "",/* here */)
}

function x4(f: ?{ m: { n: F } }) {
  f?.m.n(/* here */)
  f?.m.n(1,/* here */)
  f?.m.n(1, "",/* here */)
}

function x5(f: { m: ?F }) {
  f.m?.(/* here */)
  f.m?.(1,/* here */)
  f.m?.(1, "",/* here */)
}

function x6(g: ?{ f: F }) {
  if (g != null) {
    g.f(/* here */)
    g.f(1,/* here */)
    g.f(1, "",/* here */)
  }
}
