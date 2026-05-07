// @flow

import type { C } from './exports-class';
import type { P } from './exports-poly-class';
import type { C as LocalC } from './exports-class';
import type { P as LocalP } from './exports-poly-class';

declare const c: C;
//            ^ --pretty
declare const p: P<number>;
//            ^ --pretty
declare const lc: LocalC;
//            ^ --pretty
declare const lp: LocalP<number>;
//            ^ --pretty
