type Map<O> = {[K in keyof O]: 'FOO'};
type B = Map<{ FOO: null }>;
declare var b: B;

b.FOO as 'FOO'; // ok
b.FOO as 'BAR'; // error
b.FOO = 'BAR'; // error
