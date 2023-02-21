(undefined: interface {}); // ERROR
(null: interface {}); // ERROR

{
  (1: interface {}); // ERROR
  declare const x: number;
  (x: interface {}); // ERROR

  (new Number(x): interface {toFixed(): string}); // OK
}

{
  (true: interface {}); // ERROR
  declare const x: boolean;
  (x: interface {}); // ERROR

  (new Boolean(x): interface {toString(): string}); // OK
}
