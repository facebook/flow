function f<T>(foo: T, bar: T = foo): [T, T] {
  return [foo, bar];
}
