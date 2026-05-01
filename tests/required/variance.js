type A = {
  +r?: number,
  -w?: number,
  n?: number,
};

{
  declare const x: Required<A>;

  x.r as number; // OK
  x.r = 1; // ERROR

  x.w as number; // ERROR
  x.w = 1; // OK

  x.n as number; // OK
  x.n = 1; // OK
}
