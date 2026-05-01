declare class A { x?: number }
declare class B extends A {}
new B as interface { x?: number }; // OK
