declare module 'contains-unsupported-statements' {
  declare const a: number;
  if (true) {} // error
}
