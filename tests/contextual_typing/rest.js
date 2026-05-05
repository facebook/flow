function test1() {
  declare var fn: <TArguments extends $ReadOnlyArray<unknown> = $ReadOnlyArray<any>, TReturn = any>(
    implementation?: (...args: TArguments) => TReturn
  ) => JestMockFn<TArguments, TReturn>

  type JestMockFn<TArguments extends $ReadOnlyArray<unknown>, TReturn> = {
    (...args: TArguments): TReturn,
  }

  function test<T>(): () => Promise<T> {
    return fn(() => new Promise(resolve => {})); // okay -- infers Promise<T>
  }
}

function test2() {
  declare function foo<Args extends Array<unknown>>(
    selector: (state: unknown, ...args: Args) => unknown,
    ...args: Args
  ): unknown;

  const x = foo(state => state); // okay
}
