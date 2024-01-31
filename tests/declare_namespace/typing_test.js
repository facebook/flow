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
  declare export const a: number;
  if (true) {} // error
}
unsupported_statements.a as empty; // error: number ~> empty
