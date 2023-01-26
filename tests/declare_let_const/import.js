import {s, n} from "M";

(s: string); // OK
(s: empty); // ERROR

(n: number); // OK
(n: empty); // ERROR
