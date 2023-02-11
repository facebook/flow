type A = {
  +r: number,
  -w: number,
  n: number,
};

{
  declare const x: Partial<A>;

  (x.r: number | void); // OK
  x.r = 1; // ERROR

  (x.w: number | void); // ERROR
  x.w = 1; // OK

  (x.n: number | void); // OK
  x.n = 1; // OK
}
