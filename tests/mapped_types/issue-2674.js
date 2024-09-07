type Map<O> = {[K in keyof O]: 'FOO'};
type B = Map<{ FOO: null }>;
declare var b: B;

(b.FOO : 'FOO'); // ok
(b.FOO : 'BAR'); // error
b.FOO = 'BAR'; // error
