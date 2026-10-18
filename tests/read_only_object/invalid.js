{
  declare const x: Readonly<string>; // ERROR
}
{
  declare const x: Readonly<boolean>; // ERROR
}
{
  declare const x: Readonly<() => void>; // ERROR
}
{
  type Primitive = number | string;
  declare const x: Readonly<Primitive>; // ERROR
}
{
  declare const x: Partial<number>; // ERROR: still requires an object
  declare const y: Required<number>; // ERROR: still requires an object
}
