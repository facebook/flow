declare module 'contains-unsupported-statements' {
  declare const a: number;
  if (true) {} // error
  export const foo = ''; // error
  export type Bar = string; // ok
}
