declare var x: number;

if (([1, 2, 3] as ReadonlyArray<1 | 2 | 3>).includes(x)) {
  (x as 1 | 2 | 3);
}
