import type {ObjFoo} from './test';

(true: ObjFoo); // OK
(1: ObjFoo); // Error

import type {B, N, S} from './optional_export';

(true: B); // OK
(undefined: B); // OK
(1: B); // Error

(1: N); // OK
(undefined: N); // OK
(true: N); // Error

('hi': S); // OK
(undefined: S); // OK
(true: S); // Error
