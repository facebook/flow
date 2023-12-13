//@flow
import * as React from 'react';
import type {
  Foo as FooType,
  Poly as PolyType,
  AliasedFoo,
  MemoFoo as MemoFooType,
} from './component_in_type_position';
import {Foo, Poly, MemoFoo} from './component_in_type_position';

const fooElem: FooType = <Foo />;
const polyElem: PolyType<number> = <Poly prop={3} />;
const polyElemError: PolyType<string> = <Poly prop={3} />;

component Bar() {
  return null;
}
(<Bar />) as Foo; // ERROR

const aliasedFoo: AliasedFoo = fooElem;
aliasedFoo as number; // ERROR

const memoFoo1: MemoFooType =  <MemoFoo />;
const memoFoo2: MemoFoo =  <MemoFoo />;
const errMemoFoo: MemoFooType = 3; // ERROR
const errMemoFooType: MemoFooType = 3; // ERROR
