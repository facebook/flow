declare namespace both_type_and_value { // error
  declare export const bar: number;
  declare export type Baz = string;
}

both_type_and_value.bar as empty; // no error, everything is any
both_type_and_value.bar as both_type_and_value.Baz; // error: baz is any typed

declare namespace unsupported_declare_module_exports { // error
  declare module.exports: {foo: string};
}

unsupported_declare_module_exports.foo; // no error, everything is any
