function test1() {
  declare var fn: <TArguments: $ReadOnlyArray<mixed> = $ReadOnlyArray<any>, TReturn = any>(
    implementation?: (...args: TArguments) => TReturn
  ) => JestMockFn<TArguments, TReturn>

  type JestMockFn<TArguments: $ReadOnlyArray<mixed>, TReturn> = {
    (...args: TArguments): TReturn,
  }

  function test<T>(): () => Promise<T> {
    return fn(() => new Promise(resolve => {})); // okay -- infers Promise<T>
  }
}

function test2() {
  declare function foo<Args: Array<mixed>>(
    selector: (state: mixed, ...args: Args) => mixed,
    ...args: Args
  ): mixed;

  const x = foo(state => state); // okay
}
