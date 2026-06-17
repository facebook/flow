// In .ts/.d.ts files, `object` is a recognized builtin modeled as a
// structurally-empty (inexact) interface, matching TS's `object` primitive.
// It is scoped to .ts files (see js_unrecognized.js for the negative .js
// case).

// Object literals satisfy the empty interface.
const a: object = {}; // OK
const b: object = {x: 1, y: "two"}; // OK

// Class instances are objects too (interfaces accept class instances).
declare class C {
  x: number;
}
declare const c: C;
const d: object = c; // OK

// Arrays and functions are objects as well.
const e: object = [1, 2, 3]; // OK
const f: object = (x: number): number => x; // OK

// The defining property of TS's `object`: primitives are NOT objects. Unlike a
// normal empty interface (which in .ts files promotes primitives to their boxed
// wrappers), the `object` builtin rejects every primitive.
const p1: object = null; // ERROR -- null is not an object
const p2: object = undefined; // ERROR -- void is not an object
const p3: object = 1; // ERROR -- number is not an object
const p4: object = "x"; // ERROR -- string is not an object
const p5: object = true; // ERROR -- boolean is not an object
const p6: object = 1n; // ERROR -- bigint is not an object
const p7: object = Symbol(); // ERROR -- symbol is not an object

// As a structurally-empty interface, `object` exposes no properties, so
// reading one is an error.
declare const o: object;
o.foo; // ERROR -- prop-missing

// An `object`-typed export, consumed cross-file by `consumer.js`. The
// `object` builtin is not nameable in .js, but values typed by it flow across
// the module boundary -- and its primitive rejection travels with the type.
export declare function takesObject(x: object): void;
