// @flow

type T1 = 'a1' | 'b1' | 'c1';
type T2 = 'a2' | 'b2' | 'b3';

declare var t1: T1;
declare function t2(T2): void;
t2(t1); // error (function)
t1 as T2; // error (cast)
