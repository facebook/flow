type X = (x: {name: string}["name"]) => void;
declare var foo: X & X;
foo(1);
