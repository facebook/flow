import {x} from './exported';
import y from './exported';

(x : bigint); // ok
(x : empty); // error

(y : bigint); // ok
(y : empty); // error
