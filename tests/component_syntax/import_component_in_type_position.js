//@flow
import * as React from 'react';
import type {
  Foo as FooType,
  Poly as PolyType,
  AliasedFoo,
  MemoFoo as MemoFooType,
} from './component_in_type_position';
import {Foo, Poly, MemoFoo} from './component_in_type_position';

component Bar() {
  return null;
}
(<Bar />) as Foo; // ERROR

const rendersMemoFoo1: renders MemoFooType =  <MemoFoo />;
const rendersMFoo2: renders MemoFoo =  <MemoFoo />;
const errRendersMFoo: renders MemoFooType = 3; // ERROR
const errRendersMFooType: renders MemoFooType = 3; // ERROR
