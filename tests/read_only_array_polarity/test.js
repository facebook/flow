/* @flow */

class Foo<+T> {
  good(x: number): ReadonlyArray<T> {
    return []; // Fine, covariant can flow to covariant
  }
}

class Bar<T> {
  good(x: number): ReadonlyArray<T> {
    return []; // Fine, Invariant can flow to covariant
  }
}

class Baz<-T> {
  bad(x: number): ReadonlyArray<T> {
    // Error: T in covariant position
    return [];
  }
}

class ContraBaz<-T> {
  bad(x: (ReadonlyArray<T>) => number): number {
    // Error: T used covariantly
    // x is used contravariantly, the ROArray is used contravariantly in x,
    // double negation makes covariant!
    return 0;
  }

  good(x: (ContraBaz<T>) => number): number {
    return 0;
  }

  good2(x: ReadonlyArray<T>): number {
    return 0;
  }
}

declare var x: ReadonlyArray<number>;
x[0] = 42;
x[0 as any] = 42;
