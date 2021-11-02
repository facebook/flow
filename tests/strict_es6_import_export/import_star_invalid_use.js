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
const {'named1': renamed2} = Foo;
let x = 0;
({named1: x}) = Foo;
({'named1': x}) = Foo;

// Error on invalid use of module object in destructuring
const {y = Foo} = Foo;

// Error on use of rest element in destructuring
const {named1: renamed3, ...rest} = Foo;
let z = {};
({named1: x, ...z}) = Foo;

// Error on use of computed property in destructuring pattern
const {named1: renamed4, [1 + 2]: computed1} = Foo;
({named1: x, [1 + 2]: z}) = Foo;

 type PitchWizardDataAction = $ReadOnly<{
  type: typeof Foo,
  ...
}>;
