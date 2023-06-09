declare const x: string;
(x: string); // OK
(x: empty); // ERROR

{
  declare const x: number;
  (x: number); // OK
  (x: string); // ERROR

  x = 1; // ERROR
}

x = "foo"; // ERROR

{
  declare const y: number;
}
{
  y; // error
}
