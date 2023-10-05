import {Foo, MaybeNumber, StarNumber} from './export'
import type {RendersFoo, RendersMaybeFoo, RendersStarFoo} from './export'
import * as React from 'react';
(<Foo />: RendersFoo); // OK

component Bar() { return null }
(<Foo />: renders React.Element<typeof Bar>); // ERROR
(<Bar />: renders React.Element<typeof Foo>); // ERROR

(<MaybeNumber />: renders number); // ERROR
(<MaybeNumber />: renders? number); // OK
(<MaybeNumber />: renders* number); // OK

(<StarNumber />: renders number); // ERROR
(<StarNumber />: renders? number); // ERROR
(<StarNumber />: renders* number); // OK
