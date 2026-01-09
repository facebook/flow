declare var _: any;

function test_simple() {
  declare class C<T> {}

  declare function f<T: string>(literalValue: T): C<T>;
  declare function g<T: string>(literalValue: C<T>): C<C<T>>;

  const x0: C<'a'> = f('a');
  const x1: C<string> = f('a');
  const x2 = g(f('a'));
  const x3: C<C<string>> = g(f('a'));
  const x4: C<C<'a'>> = g(f('a')); // okay

  const y0: C<'b'> = f('a'); // error 'a' ~> 'b'
  const y1: C<C<'b'>> = g(f('a')); // error 'a' ~> 'b'
}

function test_poly_types_1() {
  declare function f<T>(literalValue: T): {f: <V>(x: V) => {f: T}};
  declare function g<T>(literalValue: T): {f: T};

  const x1 = g(f(42));
  x1.f.f('blah').f = 1;

  const x2: {f: {f: <V>(x: V) => {f: 42}}} = g(f(42)); // okay
  const x3: {f: {f: <V>(x: V) => {f: 43}}} = g(f(42)); // error

  const x4 = g(f({f:[42]}));
  x4.f.f('blah').f.f[0] = 1; // okay

  const x5: {f: {f: <V>(x: V) => {f: {f: [42]}}}} = g(f({f:[42]})); // okay
  const x6: {f: {f: <V>(x: V) => {f: {f: [43]}}}} = g(f({f:[42]})); // error
}

function test_poly_types_2() {
  declare function f<T>(literalValue: T, callback: (T) => void): {f: <V>(x: V) => {f: T}};
  declare function g<T>(literalValue: T): {f: T};

  const x1 = g(f(42, (x: 42) => { x as number; })); // okay
}

function test_mapped_types() {
  declare function f<T>(literalValue: T): $ReadOnly<{[key in keyof T]: T[key]}>;
  declare function g<T>(literalValue: T): {f: T};

  const x1 = f({f: 1, g: 2});
  type T1 = {+f: number, +g: number};
  x1 as T1; // okay
  _ as T1 as typeof x1; // okay

  const x2 = g(f({f: 1, g: 2}));
  type T2 = {f: {+f: number, +g: number}};
  x2 as T2; // okay
  _ as T2 as typeof x2; // okay
}

function test_regression() {
  declare function literal<T: string>(literalValue: T): Wrapper<T>;
  declare function union<V>(
    ...wrappers: $ReadOnlyArray<Wrapper<V>>
  ): Wrapper<V>;
  declare function object<Wrappers: {+[key: string]: Wrapper<mixed>}>(
    wrappers: Wrappers,
  ): Wrapper<$ReadOnly<MapWrapperObject<Wrappers>>>;

  type Wrapper<+V> = (value: mixed) => $ReadOnly<{value: V}>;
  type MapWrapper<C> = C extends null | void
    ? C
    : C extends Wrapper<infer T>
      ? T
      : empty;

  type MapWrapperObject<Wrappers> = {
    [K in keyof Wrappers]: MapWrapper<Wrappers[K]>,
  };

  const example0 = object({format: literal('A')});
  const example1: Wrapper<{+format: 'A'}> = object({format: literal('A')}); // okay

  type Params = $ReadOnly<{format: 'A' | 'B'}>;
  const example2: Wrapper<Params> = object({ // okay
    format: union(literal('A'), literal('B')),
  });
  const example3: Wrapper<Params> = object({ // error 'C' ~> 'A' | 'B'
    format: union(literal('A'), literal('B'), literal('C')),
  });
}
