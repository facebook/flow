// @flow


import type {B} from './export-commonjs.js';
import type C from './export-commonjs-default.js';
import type D from './export.js';
import type {E, F} from './export.js';

declare var b: B;
(b: C); // Error
