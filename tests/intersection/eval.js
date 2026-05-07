type X = (x: {name: string}["name"]) => void;
declare const foo: X & X;
foo(1);
