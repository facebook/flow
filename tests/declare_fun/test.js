declare function foo(x: number): string;
declare function foo(x: string): number;
declare function foo<X>(x: X): X;

foo(0) as string; // OK
foo("hello") as number; // OK
foo(false) as void; // error, boolean ~/~ undefined
