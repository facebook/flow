import {x, z, a, c} from './exported';
import y from './exported';

(x : bigint); // ok
(x : empty); // error

(y : bigint); // ok
(y : empty); // error

(z : bigint); // ok
(z : empty); // error

(a : bigint); // ok
(a : empty); // error

(c : bigint); // ok
(c : empty); // error
