//@flow
declare function f<T>(T): Array<T>;
f(3);
f((x: number) => 'string');
