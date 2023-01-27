import {l} from './let';
import {c} from './const';

(l: "let"); // OK
(l: empty); // ERROR

(c: "const"); // OK
(c: empty); // ERROR
