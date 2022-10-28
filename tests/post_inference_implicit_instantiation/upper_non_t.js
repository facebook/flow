//@flow
type OtherProps = {| foo: number |};

declare function HOC<OwnProps: {}>(
    Component: ({|...OwnProps, ...OtherProps|}) => mixed,
): OwnProps => mixed;

const x = HOC((x: {| foo: number, bar: number |}) => null);

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
