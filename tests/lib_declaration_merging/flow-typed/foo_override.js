declare module 'foo' {
  // Doesn't work since Node.js modules are orthogonal to `declare module`
  declare export interface Foo {
    b: string;
  }
}
