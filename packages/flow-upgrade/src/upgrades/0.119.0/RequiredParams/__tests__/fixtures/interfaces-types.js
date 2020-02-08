//@flow

interface A {
  (?string): void;

  fn1(x: ?string): void;

  fn2: (y: ?number) => void;
}

interface B {
  (string): void;

  fn1(x: string): void;

  fn2: (y: number) => void;
}

interface C<T> {
  (T): void;

  fn1(x: T): void;

  fn2: (y: T) => void;
}

interface D<T: number> {
  (T): void;

  fn1(x: T): void;

  fn2: (y: T) => void;
}

interface E<T: ?number> {
  <T1: T>(T1): void;

  fn1<T1: T>(x: T1): void;

  fn2: <T2: T>(y: T2) => void;
}

interface F<T: number> {
  <T1: T>(T1): void;

  fn1<T1: T>(x: T1): void;

  fn2: <T2: T>(y: T2) => void;
}
