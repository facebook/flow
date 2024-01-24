import {Foo, MaybeFoo, StarFoo} from './export'
import type {RendersFoo, RendersMaybeFoo, RendersStarFoo} from './export'
import * as React from 'react';
(<Foo />: RendersFoo); // OK

component Bar() { return null }
(<Foo />: renders Bar); // ERROR
(<Bar />: renders Foo); // ERROR

(<MaybeFoo />: renders Foo); // ERROR
(<MaybeFoo />: renders? Foo); // OK
(<MaybeFoo />: renders* Foo); // OK

(<StarFoo />: renders Foo); // ERROR
(<StarFoo />: renders? Foo); // ERROR
(<StarFoo />: renders* Foo); // OK
