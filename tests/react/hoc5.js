// @flow

import * as React from 'react';

import { MyEnhancedComponent, MyEnhancedFunctionComponent } from './hoc5-comp.js';

<MyEnhancedComponent />; // Error: Needs `a` and `b`.
<MyEnhancedComponent a={1} b={2} />; // OK
<MyEnhancedComponent a="foo" b={3} />; // Error: string ~> number

<MyEnhancedFunctionComponent />; // Error: Needs `a` and `b`.
<MyEnhancedFunctionComponent a={1} b={2} />; // OK
<MyEnhancedFunctionComponent a="foo" b={2} />; // Error: string ~> number
