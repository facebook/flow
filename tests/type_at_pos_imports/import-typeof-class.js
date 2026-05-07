// @flow

import typeof { C } from './exports-class';
import typeof { C as D } from './exports-class';

declare const c: C;
//               ^ --pretty --expand-json-output
declare const d: D;
//               ^ --pretty --expand-json-output
