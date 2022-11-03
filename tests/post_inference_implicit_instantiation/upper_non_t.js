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
}
