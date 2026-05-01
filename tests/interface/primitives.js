undefined as interface {}; // ERROR
null as interface {}; // ERROR

{
  1 as interface {}; // ERROR
  declare const x: number;
  x as interface {}; // ERROR

  new Number(x) as interface {toFixed(): string}; // OK
}

{
  true as interface {}; // ERROR
  declare const x: boolean;
  x as interface {}; // ERROR

  new Boolean(x) as interface {toString(): string}; // OK
}
{
  declare const x: symbol;
  x as interface {}; // ERROR

  // `new Symbol` is not allowed at runtime
}
