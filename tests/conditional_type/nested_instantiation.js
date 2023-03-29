{
  declare function poly<T>(T extends number ? number : string, T): T;
  poly(3, 3); // ok
}

{
  declare function poly<T>(T extends number ? number : string): T;
  poly(3); // underconstrained
}
