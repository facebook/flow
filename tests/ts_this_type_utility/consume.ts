// Call the .d.ts-declared functions with concrete arguments. We're verifying
// that `M & ThisType<T>` admits everything `M` admits -- i.e. ThisType<T> is
// treated as a structurally-empty interface that adds no constraints.

import {
  defineProperties,
  makeObject,
  withVoidThis,
  Repo,
} from "./decls";

// Object literal -- a plain object satisfies `... & ThisType<any>`.
defineProperties({}, {
  foo: { value: 1, writable: true },
  bar: { value: "x" },
}); // OK

// Class instance -- intersection with ThisType<...> must still admit class
// instances (interfaces accept class instances; object types would not).
declare class Descriptors {
  foo: { value: number };
}
declare const d: Descriptors;
defineProperties({}, d); // OK

// Vue-style call: D = {x: number}, M = {moveBy(...): void}.
makeObject({
  data: {x: 0, y: 0},
  methods: {
    moveBy(_dx: number, _dy: number): void {},
  },
}); // OK

// `ThisType<void>` admits any object literal too.
withVoidThis({a: 1, b: "x"}); // OK

// TypeORM extend pattern.
declare const r: Repo;
r.extend({
  helper(): number { return 1; },
}); // OK

// ThisType<T> standalone admits any object value (it's an empty interface).
const empty: ThisType<{a: number}> = {}; // OK
const withExtras: ThisType<{a: number}> = {a: 1, b: "x"}; // OK
