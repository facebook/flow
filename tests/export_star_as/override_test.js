// local exports override remote exports
import {C as C1} from "./local_override1";
(C1: string); // ok
(C1: number); // error

// local exports override remote exports regardless of export order
import {C as C2} from "./local_override2";
(C2: string); // ok
(C2: number); // error
