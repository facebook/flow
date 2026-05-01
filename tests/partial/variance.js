type A = {
  +r: number,
  -w: number,
  n: number,
};

{
  declare const x: Partial<A>;

  x.r as number | void; // OK
  x.r = 1; // ERROR

  x.w as number | void; // ERROR
  x.w = 1; // OK

  x.n as number | void; // OK
  x.n = 1; // OK
}
