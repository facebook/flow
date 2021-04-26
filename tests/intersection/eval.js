type X = (x: $ElementType<{name: string}, 'name'>) => void;
declare var foo: X & X;
foo(1);
