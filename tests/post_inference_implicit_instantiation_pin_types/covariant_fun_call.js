//@flow
declare function f<T>(T): T;
f(3);
f((x: number) => 'string');


declare function g<T>(T, T): T;
g(3, 'string');
g('string', (x: number) => 'string');
