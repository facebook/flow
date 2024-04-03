enum E {A, B}

{
  declare const x: E;
  x.valueOf() as string; // OK
  x.valueOf() as empty; // ERROR
}
{
  declare const x: E | string;
  x.valueOf() as string; // OK
}
{
  declare const x: EnumValue<string>;
  x.valueOf() as string; // OK
  x.valueOf() as empty; // ERROR
}
function f<T: EnumValue<string>>(x: T) {
  x.valueOf() as string; // OK
  x.valueOf() as empty; // ERROR
}
