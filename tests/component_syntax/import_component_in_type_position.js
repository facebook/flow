//@flow
import * as React from 'react';
import type {
  Foo as FooType,
  Poly as PolyType,
  AliasedFoo,
  MemoFoo as MemoFooType,
} from './component_in_type_position';
import {Foo, Poly, MemoFoo} from './component_in_type_position';

const fooElem: FooType extends React$RendersExactly<infer C> ? ExactReactElement_DEPRECATED<C> : empty = <Foo />; // OK
const polyElem: PolyType<number> extends React$RendersExactly<infer C> ? ExactReactElement_DEPRECATED<C> : empty = <Poly prop={3} />; // OK
const polyElemError: PolyType<number> extends React$RendersExactly<infer C> ? ExactReactElement_DEPRECATED<C> : empty = <Poly prop="STRING" />; // ERROR

component Bar() {
  return null;
}
(<Bar />) as Foo; // ERROR

const aliasedFoo: AliasedFoo extends React$RendersExactly<infer C> ? ExactReactElement_DEPRECATED<C> : empty = <Foo />;
aliasedFoo as number; // ERROR

const rendersMemoFoo1: renders MemoFooType =  <MemoFoo />;
const rendersMFoo2: renders MemoFoo =  <MemoFoo />;
const errRendersMFoo: renders MemoFooType = 3; // ERROR
const errRendersMFooType: renders MemoFooType = 3; // ERROR
