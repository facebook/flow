// @flow

const x = {
  C: class {},
  foo: 123,
  bar: function() {},
  obj: { nested: "foo" },
};

class Y {
  #foo() {}
  foo() {}
  bar = 123;
  #baz = "foo";
  get abc() {}
  set abc(x: string) {}
  Z = class {
    zmeth() {}
  }
}

const z = (class {
  foo() {}
});

const Z1 = class {
  foo() {}
};

const Z2 = (class Z2 {
  foo() {}
});

const Z3 = class Z3 {
  foo() {}
};

// z4 <> Z4
const z4 = class Z4 {
  foo() {}
};

declare class Z5 {
  foo(): void
}

declare export class Z6 {
  foo(): void
}

export class Z7 {
  foo() {}
}

function f1() {
  const f1_x = 123;
}

const f2 = function() {
  const f2_x = 123;
};

const f3 = function f3_expr() {
  const f3_x = 123;
};

declare function f4(): void;

declare export function f5(): void;

export function f6() {
  const f6_x = 123;
}

(function() {
  const iife_x = 123;
});

type T1 = {
  foo(): void;
  bar: { baz: string };
  [key: string]: string;
  [[call]]: void;
}

interface I1 {
  foo(): void;
  bar: { baz: string };
  [key: string]: string;
}

declare interface I2 {
  foo(): void;
}

type I3 = interface {
  foo(): void;
}

opaque type I4 = {
  foo: string;
}

declare opaque type I5: {
  foo: string;
}

declare var decl1: { foo: string };

declare export var decl2: { foo: string };

declare export default { foo: string };

export default {
  foo: 123,
};

declare module M1 {
  declare class C {}
}

declare module "M2" {
  declare class C {}
}

enum E1 {
  X = 1,
}

function loops() {
  for (let i = 0; i < 12; i++) {}
  for (let j in { x: 123 }) {}
  for (let k of [1, 2, 3]) {}
}
