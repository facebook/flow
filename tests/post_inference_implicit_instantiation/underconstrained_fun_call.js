//@flow
declare function f<T>(): T;
f();

declare function g<T>(T => mixed): T;
g((x: number) => 'string');

declare function h<T>(T): T => mixed;
h(3);

declare function i<T>(): Array<T>;
i();
