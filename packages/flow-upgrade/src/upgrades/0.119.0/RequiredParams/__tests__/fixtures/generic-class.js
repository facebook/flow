class A1<T> {
  fn1<T>(a: T) {};
  static fn2<T>(a: T) {};
  fn3<T: number>(a: T) {};
  fn4<T: number | void>(a: T) {};
  fn5<T: number>(a: ?T) {};
  fn6<T: ?number>(a: T) {};
  static fn7<T>(a: T, number) {};
  constructor(x: T) {}
}

class A2<T: string | number> {
  fn1(a: T) {}
  static fn2(a: T) {}
  fn3(a: T) {}
  fn4(a: T) {}
  fn5(a: ?T) {}
  fn6(a: T) {}
  static fn7(a: T, number) {}
  constructor(x: T) {}
}

class A3<T: ?number> {
  fn1(a: T) {}
  static fn2(a: T) {}
  fn3(a: T) {}
  fn4(a: T) {}
  fn5(a: ?T) {}
  fn6(a: T) {}
  static fn7(a: T, number) {}
  constructor(x: T) {}
}

type T1 = ?number;
class A4<T: T1> {
  fn1 = (a: T) => {};
  static fn2 = (a: T) => {};
  static fn3<T2>(a: T2) {}
  static fn4<T3: number>(a: T3) {}
  static fn5<T: number>(a: T) {}
  constructor(x: T) {}
}