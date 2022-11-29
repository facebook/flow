//@flow
type OtherProps = {| foo: number |};

declare function HOC<OwnProps: {}>(
    Component: ({|...OwnProps, ...OtherProps|}) => mixed,
): OwnProps => mixed;

const x = HOC((x: {| foo: number, bar: number |}) => null); // ok

declare function ReposLowerTRegressionTest<T>({o?: T}): T;
ReposLowerTRegressionTest({}) // Error: T under constrained.

declare function ReadOnly<T>($ReadOnly<T>): T;
ReadOnly({foo: 3}); // ok
declare function Partial<T>($Partial<T>): T;
Partial({foo: 3}); // ok
declare function NonMaybeType<T>($NonMaybeType<T>): T;
NonMaybeType(1); // ok

declare function IndexedAccess1<T>(T['f']): T;
IndexedAccess1(1); // Error: T under constrained.
declare function IndexedAccess2<T>(T[number]): T;
IndexedAccess2(1); // Error: T under constrained.
declare function Call<T>($Call<T, <V>(V)=>void>): T;
Call(undefined); // Error: T under constrained.
declare function ObjMap<T>($ObjMap<T, <V>(V)=>void>): T;
ObjMap({}); // Error: T under constrained.
declare function Values<T>($Values<T>): T;
Values(3); // Error: T under constrained.
declare function ElementRef<T>(React$ElementRef<T>): T;
ElementRef(1); // Error: T under constrained.
declare function ElementProps<T>(React$ElementProps<T>): T;
(ElementProps({foo: 3}): React$AbstractComponent<{foo: number}, mixed>); // ok
declare function ElementConfig<T>(React$ElementConfig<T>): T;
(ElementConfig({foo: 3}): React$AbstractComponent<{foo: number}, mixed>); // ok
declare function Exact<T>($Exact<T>): T;
Exact({foo: 3}); // ok

function spreads() {
  type T = {|bar: number, baz: number|};

  declare function spread1<Props>({...Props}): Props;
  const s1 = spread1({foo: 3, bar: 2, baz: 1}); // ok
  (s1.foo: number);
  (s1.bar: number);
  (s1.baz: number);
  s1.bad; // error in LTI
  declare function spread2<Props>({...Props, bar: number, baz: number}): Props;
  const s2 = spread2({foo: 3, bar: 2, baz: 1}); // ok
  (s2.foo: number);
  s2.bad; // error in LTI
  s2.bar; // error in LTI
  s2.baz; // error in LTI
  declare function spread3<Props>({...Props, ...T}): Props;
  const s3 = spread3({foo: 3, bar: 2, baz: 1}); // ok
  (s3.foo: number);
  s3.bad; // error in LTI
  s3.bar; // error in LTI
  s3.baz; // error in LTI
  declare function spread4<Props>({baz: number, bar: number, ...Props}): Props;
  const s4 = spread4({foo: 3, bar: 2, baz: 1}); // ok
  (s4.foo: number);
  s4.bad; // error in LTI
  s4.bar; // error in LTI
  s4.baz; // error in LTI
  declare function spread5<Props>({...T, ...Props}): Props;
  const s5 = spread5({foo: 3, bar: 2, baz: 1}); // ok
  (s5.foo: number);
  s5.bad; // error in LTI
  s5.bar; // error in LTI
  s5.baz; // error in LTI
  declare function spread6<Props>({hhh: number, ...Props, ...T}): Props;
  const s6 = spread6({foo: 3, bar: 2, baz: 1, hhh: 0}); // ok
  (s6.foo: number);
  s6.bad; // error in LTI
  s6.bar; // error in LTI
  s6.baz; // error in LTI

  declare function spread_and_readonly<Props>($ReadOnly<{...Props}>): Props;
  const sr = spread_and_readonly({foo: 1, bar: ''});
  (sr.foo: number);
  (sr.bar: string);
  sr.bad; // error in LTI

  declare var cp: {|bar: number|};
  declare function optional<P>($ReadOnly<{|cp: P, foo?: string, ...P|}>): P;
  const o = optional({cp, foo: '', bar: 3}); // ok
  o.cp; // error in LTI
  o.foo; // error in LTI
  (o.bar: number); // error in LTI
}

function rests() {
  declare function ArrRest<TArgs: $ReadOnlyArray<mixed>>(...TArgs): TArgs;
  const r1 = ArrRest(...([]: Array<string>)); // ok
  const r2 = ArrRest(...([1, 2]: [1, 2])); // ok
  (r1: Array<string>); // ok
  (r1[65536]: string); // ok
  r1.bad; // error
  (r2: [1, 2]); // ok
  (r2[0]: 1); // ok
  (r2[1]: 2); // ok
  (r2[2]: empty); // error
  r2.bad; // error
}

function ResolveSpreadsToMultiflowSubtypeFull() {
  declare function f<TArguments: $ReadOnlyArray<mixed>>(fn: (...TArguments) => mixed): (...TArguments) => mixed

  function g(x: number): void {}
  const f1 = f(g);
  f1(0); // ok
  f1(""); // error

  function h(x: number, y: string): void {}
  const f2 = f(h);
  f2(0, ""); // ok
  f2("", 0); // error

  function i(...rest: Array<number>): void {}
  const f3 = f(i);
  f3(0, 1); // ok
  f3("") // error
}

function Diffs() {
  declare function Diff0<T>(): $Diff<T, {foo: number}>;
  Diff0(); // error
  declare function Diff1<T>($Diff<T, {|foo: string|}>): T;
  const d1 = Diff1({bar: 3});
  (d1.foo: string);
  (d1.bar: number);
  (d1: {|foo: string, bar: number|});
  (d1: {|foo: string | number, bar: number|}); // error
  d1.bad; // error
  (d1: empty); // error
  declare function Diff2<T>($Diff<T, {|foo?: string|}>): T;
  const d2 = Diff2({bar: 3});
  (d2.foo: ?string);
  (d2.bar: number);
  d2.bad; // error
  (d2: empty); // error
  declare function Diff3<T>($Diff<T, {foo: string, ...}>): T;
  const d3 = Diff3({bar: 3});
  (d3.foo: string);
  (d3.bar: number);
  d3.bad; // error
  (d3: empty); // error
  (d3: {|foo: string, bar: number|}); // error
  declare function Diff4<T>($Diff<T, {[string]: number}>): T;
  const d4 = Diff4({bar: 3});
  Diff4({bar: ''}); // error: number is incompatible with string
  (d4.foo: number);
  (d4.boz: number);
  (d4.bar: number);
  d4[0]; // error
  (d4: {|[string]: number, bar: number|});
}

type BaseProps<T> = {|v: T|};
declare function ResolveUnion<T: React$Key>({|...BaseProps<T>, foo: string|}): T;
const resolved_union_result = ResolveUnion({v: 3, foo: ''});
(resolved_union_result: number);
