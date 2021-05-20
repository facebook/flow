declare class A { x?: number }
declare class B extends A {}
(new B: interface { x?: number }); // OK
