// @flow

declare function get(key: string): void;
declare function get(key: number): void;
get(true); // incompatible-call

declare var x: null & 3;

x(); // not-a-function

declare var f: null & (number => void);
f(true); // incompatible-call

declare var y : number & string;
(y : boolean) // incompatible-cast
