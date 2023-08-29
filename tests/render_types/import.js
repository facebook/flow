import {Foo, type RendersFoo} from './export'
import * as React from 'react';
(<Foo />: RendersFoo); // OK

component Bar() { return null }
(<Foo />: renders Bar); // ERROR
(<Bar />: renders Foo); // ERROR
