// @flow

import typeof { C } from './exports-class';
import typeof { C as D } from './exports-class';

declare var c: C;
//             ^ --pretty --expand-json-output
declare var d: D;
//             ^ --pretty --expand-json-output
