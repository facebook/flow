declare module 'test_lib' {
  declare export var foo: string;
}

declare module 'Unknown_LibDeclared' {
  declare module.exports: string;
}

declare module 'Untyped_LibDeclared' {
  declare module.exports: string;
}
