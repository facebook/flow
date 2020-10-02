// @flow

import {o} from "./cycle1";

if (o.q) {} // error: o.q does not exist

export const o_loop = o;
