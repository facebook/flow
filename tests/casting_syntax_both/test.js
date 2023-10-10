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

// Colon-style cast allowed
{
  const x = 1;
  (x: number); // OK
}
{
  const x = 1;
  (x: boolean); // ERROR
}
