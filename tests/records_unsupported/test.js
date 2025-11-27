record R {} // ERROR: unsupported

const x = R {}; // ERROR: unsupported

import {RNamed} from './exported';

const y = RNamed {a: 1}; // ERROR: unsupported
y as empty; // OK
