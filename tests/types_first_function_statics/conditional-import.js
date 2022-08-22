// @flow

import {f, g} from './conditional';

(f.legal: number); // OK
(f.a: number); // ERROR
(f.b: number); // ERROR

(g.legal: number); // OK
(g.a: number); // ERROR
