// @flow

import {foo} from './dep';

if (foo) { // sketchy null string (cross-module)
}

const bar: string | void = undefined;
if (bar) { // sketchy null string (local)
}
