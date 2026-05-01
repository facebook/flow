import type { B as B1 } from './test1';
const b1 = require('./test1');

import type { B as B2 } from './test2';
const b2 = require('./test2');

b1 as B1; // Error number ~> string
b1.x as string; // Error number ~> string
b1.y as string; // Error, y may not exist, number ~> string
b1.z as string; // Error, z may not exist, number ~> string

b2 as B2; // Error, number ~> string
b2.x as string; // Error number ~> string
b2.z as string; // Error number ~> string

const b3 = require('./test3');
b3 as {| |}; // Error inexact -> exact
