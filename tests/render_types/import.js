import {Foo, MaybeFoo, StarFoo} from './export'
import type {RendersFoo, RendersMaybeFoo, RendersStarFoo} from './export'
import * as React from 'react';
<Foo /> as RendersFoo; // OK

component Bar() { return null }
<Foo /> as renders Bar; // ERROR
<Bar /> as renders Foo; // ERROR

<MaybeFoo /> as renders Foo; // ERROR
<MaybeFoo /> as renders? Foo; // OK
<MaybeFoo /> as renders* Foo; // OK

<StarFoo /> as renders Foo; // ERROR
<StarFoo /> as renders? Foo; // ERROR
<StarFoo /> as renders* Foo; // OK
