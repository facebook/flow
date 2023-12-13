//@flow
import * as React from 'react';
export component Foo() { return null }

const x: Foo = Foo; // ERROR
const fooElem: Foo = <Foo />; // OK

export component Poly<T>(prop: T) { return null }
const polyElem: Poly<number> = <Poly prop={3} />; // OK
const polyElemBad: Poly<number> = <Poly prop="STRING" />; // ERROR
type PolyNoTargs = Poly; // ERROR

export type AliasedFoo = Foo;

export const MemoFoo = (React.memo(Foo): React.AbstractComponent<React.PropsOf<Foo>, React.RefOf<Foo>, renders Foo>);
const FooElem: renders Foo = <MemoFoo />;
const MemoFooElem: MemoFoo = <MemoFoo />;
