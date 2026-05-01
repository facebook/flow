declare const x: string;
x as string; // OK
x as empty; // ERROR

{
  declare const x: number;
  x as number; // OK
  x as string; // ERROR

  x = 1; // ERROR
}

x = "foo"; // ERROR

{
  declare const y: number;
}
{
  y; // error
}
