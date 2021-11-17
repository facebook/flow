// @flow

// this file is a dependent of a.js and should be rechecked when
// a.js is focused.

import {y} from './a';

(y: void); // error
