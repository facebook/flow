import {Foo, type RendersFoo} from './export'
import * as React from 'react';
component Bar() { return null }
declare const x: $Renders<Foo>;
(x: $Renders<Bar>); // ERROR
