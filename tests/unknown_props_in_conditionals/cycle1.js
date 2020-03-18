// @flow

import {o_loop} from "./cycle2";

if (o_loop.q) {} // error: o_loop.q does not exist (NB: this is `o` exported from here)

export const o = { p: 0 };
