// @flow

import * as Foo from './foo';
import React from 'react';

// Error on direct use of module object
Foo;
Foo();
const Foo2 = Foo;
let Foo3: Foo;
<Foo></Foo>;

// Dynamic member accesses are not allowed
let prop = 'named1';
Foo[prop];

// Static member accesses are allowed
Foo.named1;
Foo['named1'];
let Foo4: Foo.TypeExport;
<Foo.ComponentExport></Foo.ComponentExport>;

// Destructuring is allowed
const {named1: renamed1} = Foo;
let x = 0;
({named1: x}) = Foo;

// Error on invalid use of module object in destructuring
const {y = Foo} = Foo;
