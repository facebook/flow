declare namespace both_type_and_value {
  declare export const bar1: number;
  declare export const bar2: boolean;
  declare export type Baz = string;
}

both_type_and_value.bar1 as empty; // error: number ~> empty
both_type_and_value.bar2 as empty; // error: boolean ~> empty
both_type_and_value.bar1 as foo.Baz; // error: number ~> string

declare namespace unsupported_declare_module_exports {
  declare module.exports: {foo: string}; // unsupported
}

unsupported_declare_module_exports.foo; // prop-missing because `declare module.exports` is ignored

declare namespace unsupported_statements {
  declare const a: number;
  enum B {
    C,
    D,
  }
  if (true) {} // error
  import React from 'react'; // unsupported
}
unsupported_statements.a as empty; // error: number ~> empty
unsupported_statements.B.C as unsupported_statements.B; // ok
unsupported_statements.B.D as empty; // error: enum ~> empty

import {exported_ns} from './exported';
exported_ns.bar1 as empty; // error: number ~> empty
exported_ns.bar2 as empty; // error: boolean ~> empty
exported_ns.bar3 as empty; // error: string ~> empty
exported_ns.f(3) as empty; // error: number ~> empty
1 as exported_ns.Baz; // error: number ~> string
exported_ns.B.C as empty; // error: enum ~> empty
exported_ns.React; // error: prop-missing
