//@flow
import * as React from 'react';
export component Foo() { return null }

const x: Foo = Foo; // ERROR
const fooElem: Foo = <Foo />; // ERROR
const fooRenderShorthand: renders Foo = <Foo />; // OK
const fooRenderVerbose: renders React$RendersExactly<typeof Foo> = <Foo />; // OK

export component Poly<T>(prop: T) { return null }
type PolyNoTargs = Poly; // ERROR

export type AliasedFoo = Foo;

export const MemoFoo: component(...React.PropsOf<Foo>) renders Foo = React.memo(Foo);
const rendersFoo: renders Foo = <MemoFoo />;
const rendersMemoFoo: renders MemoFoo = <MemoFoo />;
