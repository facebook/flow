{
  // list of args
  declare const tag: (quasis: TaggedTemplateLiteralArray, x: number, y: number) => string;
  tag`foo${1}bar${2}`; // OK
  tag`foo${'bad'}bar${2}`; // ERROR: string !~> number
}

{
  // wrong arity
  declare const tag: (quasis: TaggedTemplateLiteralArray, number: number) => string;
  tag`foo${1}bar${2}`; // ERROR: expected 2 args, got 3
}

{
  // rest expr
  declare const tag: (quasis: TaggedTemplateLiteralArray, ...exprs: Array<number>) => string;
  tag`foo${1}`; // OK
  tag`foo${'bad'}`; // ERROR: string !~> number
}

{
  // raw property
  const tag = (quasis: TaggedTemplateLiteralArray, x: number) => {
    (quasis.raw: $ReadOnlyArray<string>); // OK
    (quasis.raw: empty); // ERROR
  };
  tag`foo${1}`; // OK
}

{
  // `String.raw`
  const x = String.raw`foo${1}bar`;
  (x: string); // OK
  (x: empty); // ERROR

  String.raw('foo'); // ERROR
}

{
  // `String.raw` with object
  const x = String.raw({
    raw: ['test']
  });
  (x: string); // OK
  (x: empty); // ERROR
}
