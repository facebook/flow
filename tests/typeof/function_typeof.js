/**
 * @flow
 */

export function f1(x: typeof x) {} // error - unbound x
export function f2(x: number, y: typeof x) {}
export function f3(x: {f: number}, y: typeof x.f) {}
export function f4(x: {f: Array<number>}, ...y: typeof x.f) {}

export function poly1<X>(x: X, y: typeof x) {}
export function poly2<X: {f: number}>(x: X, y: typeof x.f) {}
export function poly3<V, X: {f: V}>(x: X, y: typeof x.f) {}

f1(1); // okay
f1('a'); // okay

f2(1, 2); // okay
f2(1, 'a'); // error

f3({f: 1}, 2); // okay
f3({f: 1}, 'a'); // error

f4({f: [1, 2, 3]}, 4, 5, 6);
f4({f: [1, 2, 3]}, '1'); // error

poly1(1, 2); // TODO okay
poly1(1, 'a'); // error (TODO string ~> number)

poly2({f: 1}, 3); // okay
poly2({f: 1}, 'a'); // error string ~> number

poly3({f: 1}, 3); // TODO okay
poly3({f: 1}, 'a'); // error (TODO string ~> number)
