type A = { tag: 1 };
type B = { tag: 2 };

type P<T> = A | B;
declare var x: P<any> & {};
x as P<any>;
