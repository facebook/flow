declare let x: string;
x as string; // OK
x as empty; // ERROR

{
  declare let x: number;
  x as number; // OK
  x as string; // ERROR

  x = 1; // OK
}

x = "foo"; // OK
