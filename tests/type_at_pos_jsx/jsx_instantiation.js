import * as React from 'react';

declare component Foo1<T>(x: T, y: T);
//                ^
declare component Foo2<T, S>(x: T, y: S);
declare component Foo3<T: number>(x: T, y: T);
declare component Foo4<T, S: string>(x: T, y: S);

function test_Nominal() {
    <Foo1 x={1} y={""} />;
//   ^
}

function test_Explicit() {
    <Foo1<number | string> x={1} y={""} />;
//   ^
}

function test_Nominal_error() {
    <Foo4 x={""} y={1} />; // error, but shows instantiation
//   ^
}

component Test_return_hint() {
    declare component Bar<T>(...props: $ReadOnly<{ n: T }>);
    return <Bar n={1} />;
//          ^
}

function test_Expose_Any_Propagation_Bug() {
    type Q = $ReadOnly<{prop: number}>;

    type Props<TQuery: Q> = $ReadOnly<{
        f: TQuery['prop'] => void,
    }>;

    declare component Foo<TQuery: Q>(...props: Props<TQuery>);

    component Bar() {
        // should have been an error, instead we see that we've inferred 'any'
        return <Foo f={(x: string) => {}} />;
//              ^
    }
}

// The cases below are less likely. We are still including them to ensure that
// we are recording the correct branch properly instantiated. Note that the
// syntax "component C<Type>" is toplevel only, so when we have a toplevel union
// or intersection of components, we fall back to React$AbstractComponent<...>.

function test_Nominal_union(
    Bar1: (typeof Foo1) | (typeof Foo2),
    Bar2: (typeof Foo1) | (typeof Foo3),
) {
    <Bar1 x={""} y = {1} />;
//   ^
    <Bar2 x={""} y = {1} />;
//   ^
}

function test_Nominal_inter(
    Bar12: (typeof Foo1) & (typeof Foo2),
    Bar23: (typeof Foo2) & (typeof Foo3),
    Bar34: (typeof Foo3) & (typeof Foo4),
    Bar32: (typeof Foo3) & (typeof Foo2),
) {
   <Bar12 x={""} y = {1} />;
//  ^
   <Bar23 x={""} y = {1} />;
//  ^
   <Bar34 x={""} y = {1} />;
//  ^
   <Bar32 x={""} y = {1} />;
//  ^
}

function test_Nominal_intersection_nested(
    Bar323: ((typeof Foo3) & (typeof Foo2)) & typeof Foo3,
    Bar342: (typeof Foo3) & ((typeof Foo4) & (typeof Foo2)),
) {
    <Bar323 x={""} y = {1} />;
//   ^
    <Bar342 x={""} y = {1} />;
//   ^
}

function test_Nominal_union_of_inter(
    Bar: (typeof Foo1 & typeof Foo3) | (typeof Foo2 & typeof Foo4),
) {
    <Bar x={""} y = {1} />;
//   ^
}

function test_Nominal_inter_of_union(
    Bar: (typeof Foo1 | typeof Foo2) & (typeof Foo3 | typeof Foo4),
) {
    <Bar x={""} y = {1} />; // TODO should show instantiation for both Foo1 and Foo2
//   ^
}

function test_func() {
    type Props<T> = $ReadOnly<{ x: T, y: T }>;
    declare function Foo<T>(props: Props<T>): React.Node;

    <Foo x={1} y={""} />;
//   ^

    declare var Indirection: {f: typeof Foo}['f'];
    <Indirection x={1} y={""} />;
//   ^
}
