import * as React from 'react';

import {Foo, Bar, Baz} from './include';

const x = <Foo x={42}/>; // error
const y = <Bar />; //error
const z = <Baz z={42} />; // error

import {FooX, BarX, BazX} from './exclude';

const w = <FooX x={42}/>; // ok
const u = <BarX />; //ok
const v = <BazX z={42} />; // ok
