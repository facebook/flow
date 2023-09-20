//@flow
import * as React from 'react';
import type {Foo as FooType, Poly as PolyType} from './component_in_type_position';
import {Foo, Poly} from './component_in_type_position';

const fooElem: FooType = <Foo />;
const polyElem: PolyType<number> = <Poly prop={3} />;
const polyElemError: PolyType<string> = <Poly prop={3} />;


component Bar() { return null }
(<Bar />: Foo); // ERROR
