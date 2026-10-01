declare class T {}
declare class U {}

declare const o1: {...{[string]:T},...{p:U, ...}, ...}; // OK
o1 as {p?:T|U,[string]:T}; // Error: p is invariant
