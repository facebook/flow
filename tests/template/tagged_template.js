{
  // list of args
  declare const tag: (quasis: Array<string>, x: number, y: number) => string;
  tag`foo${1}bar${2}`; // OK
  tag`foo${'bad'}bar${2}`; // ERROR: string !~> number
}

{
  // wrong arity
  declare const tag: (quasis: Array<string>, number: number) => string;
  tag`foo${1}bar${2}`; // ERROR: expected 2 args, got 3
}

{
  // rest expr
  declare const tag: (quasis: Array<string>, ...exprs: Array<number>) => string;
  tag`foo${1}`; // OK
  tag`foo${'bad'}`; // ERROR: string !~> number
}
