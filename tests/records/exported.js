record R {
  a: number,
}

export type ReadonlyR = Readonly<R>; // ERROR
