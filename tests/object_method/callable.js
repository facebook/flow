//@flow
type O = {
    <T, U>(): T,
 ...}

declare const o: O;

o<number>(); // Error should be here

interface I {
    <T, U>(): T
}

declare const i: I;

i<number>(); // Error should be here
