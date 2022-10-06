//@flow
type O = {
    <T, U>(): T,
}

declare var o: O;

o<number>(); // Error should be here

interface I {
    <T, U>(): T
}

declare var i: I;

i<number>(); // Error should be here
