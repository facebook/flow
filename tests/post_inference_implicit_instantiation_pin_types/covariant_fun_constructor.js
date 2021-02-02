//@flow
declare function f<T>(T): T;
new f(3);
new f((x: number) => 'string');


declare function g<T>(T, T): T;
new g(3, 'string');
new g('string', (x: number) => 'string');
