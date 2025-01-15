declare module 'test_lib' {
  declare export var foo: string;
  declare export var bar: {baz: string};
  declare export const array: Array<string>;
  declare export const tuple: [string];
  declare export const dict: {[string]: string};
}

declare module 'test_lib_cjs' {
  declare module.exports: {
    foo: string,
    bar: {baz: string},
  }
}

declare module 'Unknown_LibDeclared' {
  declare module.exports: string;
}

declare module 'Untyped_LibDeclared' {
  declare module.exports: string;
}
