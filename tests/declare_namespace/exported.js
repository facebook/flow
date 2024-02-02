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

export {exported_ns}; // TODO: support declare export namespace syntax

exported_ns.bar1 as empty; // error: number ~> empty
exported_ns.bar2 as empty; // error: boolean ~> empty
exported_ns.bar3 as empty; // error: string ~> empty
exported_ns.f(3) as empty; // error: number ~> empty
1 as exported_ns.Baz; // error: number ~> string
exported_ns.B.C as empty; // error: enum ~> empty
exported_ns.React; // error: prop-missing
