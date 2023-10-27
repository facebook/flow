// @flow

type F =
  & (() => void)
  & ((x: string) => void)
  & (<A>(x: A, y: A) => void);

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

// Nested overloads

type G<V> =
  & (() => void)
  & ((x: V) => void);

type H =
  & G<string>
  & (<A>(x: A, y: A) => void);

function y0(f: H) {
  f(/* here */)
}

function y1(f: H | void) {
  f?.(/* here */)
}

function y2(f: ?{ m: H }) {
  f?.m(/* here */)
}

function y3(f: ?{ [key: string]: H }) {
  f?.["abc"](/* here */)
}

function y4(f: ?{ m: { n: H } }) {
  f?.m.n(/* here */)
}

function y5(f: { m: ?H }) {
  f.m?.(/* here */)
}

function y6(g: ?{ f: H }) {
  if (g != null) {
    g.f(/* here */)
  }
}

// Error cases - no suggestions

function x0(f: ?F, h: ?H) {
  f(/* here */)
  h(/* here */)
}
