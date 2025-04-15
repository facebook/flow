//@flow
type OtherProps = {|foo: number|};

declare function HOC<OwnProps: {}>(
  Component: ({|...OwnProps, ...OtherProps|}) => mixed,
): OwnProps => mixed;

const x = HOC((x: {|foo: number, bar: number|}) => null); // ok

declare function ReposLowerTRegressionTest<T>({o?: T}): T;
ReposLowerTRegressionTest({}); // Error: T under constrained.

declare function ReadOnly<T>($ReadOnly<T>): T;
ReadOnly({foo: 3}); // ok
declare function partial<T>(Partial<T>): T;
partial({foo: 3}); // ok
declare function NonMaybeType<T>($NonMaybeType<T>): T;
NonMaybeType(1); // ok

declare function IndexedAccess1<T>(T['f']): T;
IndexedAccess1(1); // Error: T under constrained.
declare function IndexedAccess2<T>(T[number]): T;
IndexedAccess2(1); // Error: T under constrained.
declare function ConditionalType<T>(T extends mixed ? void : empty): T;
ConditionalType(undefined); // Error: T under constrained.
declare function MappedType<T>({[K in keyof T]: void}): T;
MappedType({}); // Error: T under constrained.
declare function Values<T>($Values<T>): T;
Values(3); // Error: T under constrained.
declare function ElementRef<T>(React.ElementRef<T>): T;
ElementRef(1); // Error: T under constrained.
declare function ElementProps<T>(React.ElementProps<T>): T;
ElementProps({foo: 3}) as React.ComponentType<
  {foo: number},
>; // ok
declare function ElementConfig<T>(React.ElementConfig<T>): T;
ElementConfig({foo: 3}) as React.ComponentType<
  {foo: number},
>; // ok
declare function Exact<T>($Exact<T>): T;
Exact({foo: 3}); // ok

function spreads() {
  type T = {|bar: number, baz: number|};

  declare function spread1<Props>({...Props}): Props;
  const s1 = spread1({foo: 3, bar: 2, baz: 1}); // ok
  s1.foo as number;
  s1.bar as number;
  s1.baz as number;
  s1.bad; // error
  declare function spread2<Props>({...Props, bar: number, baz: number}): Props;
  const s2 = spread2({foo: 3, bar: 2, baz: 1}); // ok
  s2.foo as number;
  s2.bad; // error
  s2.bar; // error
  s2.baz; // error
  declare function spread3<Props>({...Props, ...T}): Props;
  const s3 = spread3({foo: 3, bar: 2, baz: 1}); // ok
  s3.foo as number;
  s3.bad; // error
  s3.bar; // error
  s3.baz; // error
  declare function spread4<Props>({baz: number, bar: number, ...Props}): Props;
  const s4 = spread4({foo: 3, bar: 2, baz: 1}); // ok
  s4.foo as number;
  s4.bad; // error
  s4.bar; // error
  s4.baz; // error
  declare function spread5<Props>({...T, ...Props}): Props;
  const s5 = spread5({foo: 3, bar: 2, baz: 1}); // ok
  s5.foo as number;
  s5.bad; // error
  s5.bar; // error
  s5.baz; // error
  declare function spread6<Props>({hhh: number, ...Props, ...T}): Props;
  const s6 = spread6({foo: 3, bar: 2, baz: 1, hhh: 0}); // ok
  s6.foo as number;
  s6.bad; // error
  s6.bar; // error
  s6.baz; // error

  declare function spread_and_readonly<Props>($ReadOnly<{...Props}>): Props;
  const sr = spread_and_readonly({foo: 1, bar: ''});
  sr.foo as number;
  sr.bar as string;
  sr.bad; // error

  declare var cp: {|bar: number|};
  declare function optional<P>($ReadOnly<{|cp: P, foo?: string, ...P|}>): P;
  const o = optional({cp, foo: '', bar: 3}); // ok
  o.cp; // error
  o.foo; // error
  o.bar as number; // error
}

function rests() {
  declare function ArrRest<TArgs: $ReadOnlyArray<mixed>>(...TArgs): TArgs;
  const r1 = ArrRest(...([] as Array<string>)); // ok
  const r2 = ArrRest(...([1, 2] as [1, 2])); // ok
  r1 as Array<string>; // ok
  r1[65536] as string; // ok
  r1.bad; // error
  r2 as [1, 2]; // ok
  r2[0] as 1; // ok
  r2[1] as 2; // ok
  r2[2] as empty; // error
  r2.bad; // error
}

function ResolveSpreadsToMultiflowSubtypeFull() {
  declare function f<TArguments: $ReadOnlyArray<mixed>>(
    fn: (...TArguments) => mixed,
  ): (...TArguments) => mixed;
  declare function params<TArguments: $ReadOnlyArray<mixed>>(
    fn: (...TArguments) => mixed,
  ): TArguments;

  function g(x: number): void {}
  const f1 = f(g);
  f1(0); // ok
  f1(''); // error

  function h(x: number, y: string): void {}
  const f2 = f(h);
  f2(0, ''); // ok
  f2('', 0); // error

  function i(...rest: Array<number>): void {}
  const f3 = f(i);
  f3(0, 1); // ok
  f3(''); // error

  const v1 = params(h);
  v1 as [number, string]; // ok
  v1 as empty; // error: tuple ~> empty

  declare function funArgRest<Rest>(f: (first: empty, second: empty, ...args: Rest) => mixed): Rest;
  declare function funArgRestInput(string, number, boolean, Date): void;
  const f4 = funArgRest(funArgRestInput);
  f4 as [+label1: boolean, +label2: Date]; // ok
  f4 as empty; // error: tuple ~> empty
}

type BaseProps<T> = {|v: T|};
declare function ResolveUnion<T: React$Key>({|
  ...BaseProps<T>,
  foo: string,
|}): T;
const resolved_union_result = ResolveUnion({v: 3, foo: ''});
resolved_union_result as number;
