// @flow

type T1 = 'a' | 'b' | 'c';
type T2 = 'a' | 'b' | 'c';

declare var t1: T1;
declare function t2(x: T2): void;

t1 as T2;
t2(t1);
