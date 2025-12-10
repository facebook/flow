record R {
  a: number,
}

export type ReadonlyR = Readonly<R>; // ERROR

export record RNamed {
  a: number,
  b: string = "",
}

export record RKeyNames {
  'foo': string,
  42: number,
}
