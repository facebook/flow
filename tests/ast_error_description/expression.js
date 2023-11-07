/**
 * @noformat
 * @flow
 */

type T = {p: mixed};

let a = 0;
const f = (x: mixed): mixed => x;
const o = {p: 42};

declare var React: {createElement: React$CreateElement};
function MyComponent() {}

(([]: empty): T).p as empty;
(([1]: empty): T).p as empty;
(([1, 2, 3]: empty): T).p as empty;
((() => {}: empty): T).p as empty;
((() => 42: empty): T).p as empty;
((() => ({}): empty): T).p as empty;
(((a = 42): empty): T).p as empty;
(((a >>>= 42): empty): T).p as empty;
((({a} = {a: 42}): empty): T).p as empty;
((([a] = [42]): empty): T).p as empty;
(((a = a = 42): empty): T).p as empty;
((1 + 2: empty): T).p as empty;
((1 + 2 + 3: empty): T).p as empty;
((1 - (2 + 3): empty): T).p as empty;
(((1 + 2) * (3 + 4): empty): T).p as empty;
((1 + 2 * (3 + 4): empty): T).p as empty;
((f(): empty): T).p as empty;
((f(42): empty): T).p as empty;
((class {}: empty): T).p as empty;
(((1 ? 2 : 3): empty): T).p as empty;
((((1 ? 2 : 3) ? 4 : 5): empty): T).p as empty;
((function () {}: empty): T).p as empty;
((a: empty): T).p as empty;
((import('fs'): empty): T).p as empty;
((<MyComponent />: empty): T).p as empty;
((<>foo</>: empty): T).p as empty;
(('foo': empty): T).p as empty;
(('abcdefghijklmnopqrstuvwxyz': empty): T).p as empty;
((42: empty): T).p as empty;
((true: empty): T).p as empty;
((false: empty): T).p as empty;
((null: empty): T).p as empty;
((o.p: empty): T).p as empty;
((new f(): empty): T).p as empty;
((new f(42): empty): T).p as empty;
(({}: empty): T).p as empty;
(((1, 2, 3): empty): T).p as empty;
class X extends class {
  m() {}
} {
  m() {
    ((super.m: empty): T).p as empty;
  }
}
((f`foobar`: empty): T).p as empty;
((`foobar`: empty): T).p as empty;
(function (this: number) {
  ((this: empty): T).p as empty;
}).call(42);
(((42: string): empty): T).p as empty;
((-42: empty): T).p as empty;
((!42: empty): T).p as empty;
((++a: empty): T).p as empty;
((a++: empty): T).p as empty;
function* g1(): Generator<number | void, void, number> {
  ((yield: empty): T).p as empty;
  ((yield 42: empty): T).p as empty;
}
function* g2(): Generator<number | void, void, number> {
  ((yield* g1(): empty): T).p as empty;
}
