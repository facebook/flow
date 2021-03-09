//@flow
declare function f<T>(): T;
new f();

declare function g<T>(T => mixed): T;
new g((x: number) => 'string');

declare function h<T>(T): T => mixed;
new h(3);

declare function i<T>(): Array<T>;
new i();
