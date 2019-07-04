// @flow

import type { B as B1 } from './test1';
const b1 = require('./test1');

import type { B as B2 } from './test2';
const b2 = require('./test2');

(b1: B1);
(b1.x: string);
(b1.y: string);
(b1.z: string);

(b2: B2);
(b2.x: string);
(b2.z: string);
