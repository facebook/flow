type A = {
  +r?: number,
  -w?: number,
  n?: number,
};

{
  declare const x: $Required<A>;

  (x.r: number); // OK
  x.r = 1; // ERROR

  (x.w: number); // ERROR
  x.w = 1; // OK

  (x.n: number); // OK
  x.n = 1; // OK
}
