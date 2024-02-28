declare namespace exported_ns {
  declare export const bar1: number;
  declare const bar2: boolean;
  declare var bar3: string;
  declare function f(string): string;
  declare function f(number): number;
  declare type Baz = string;
  enum B { C, D }
  if (true) {} // unsupported
  declare module.exports: {foo: string}; // unsupported
  import React from 'react'; // unsupported
}

declare namespace empty {}
declare namespace type_only {
  type T = number;
}

// TODO: support declare export namespace syntax
export {
  exported_ns,
  empty, // error: empty is type-only
};

export type {type_only, exported_ns as exported_ns_type_only};

exported_ns.bar1 as empty; // error: number ~> empty
exported_ns.bar2 as empty; // error: boolean ~> empty
exported_ns.bar3 as empty; // error: string ~> empty
exported_ns.f(3) as empty; // error: number ~> empty
1 as exported_ns.Baz; // error: number ~> string
exported_ns.B.C as empty; // error: enum ~> empty
exported_ns.React; // error: prop-missing
