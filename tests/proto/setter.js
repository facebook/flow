const o = {x: 1};
o.__proto__ = {}; // error: `__proto__` is missing in the object literal

class C {
  static s: number = 1;
}
new C().__proto__ = null; // error: unsupported
C.__proto__ = {}; // error: unsupported

function f() {}
f.__proto__ = {}; // ok: declares a function static

declare const anyV: any;
anyV.__proto__ = {};

declare const u: {a: number, ...} | {b: string, ...};
u.__proto__ = {}; // error: unsupported

declare const i: {a: number, ...} & {b: string, ...};
i.__proto__ = {}; // error
