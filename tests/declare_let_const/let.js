declare let x: string;
(x: string); // OK
(x: empty); // ERROR

{
  declare let x: number;
  (x: number); // OK
  (x: string); // ERROR

  x = 1; // OK
}

x = "foo"; // OK
