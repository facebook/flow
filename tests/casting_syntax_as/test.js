// `as` expression allowed
{
  const x = 1;
  x as number; // OK
}
{
  const x = true;
  x as any as number; // OK
}
{
  const x = 1;
  x as boolean; // ERROR
}

// Colon-style cast not allowed
{
  const x = 1;
  (x: number); // ERROR
}
