//@flow
declare function f<T>(T): Array<T>;
new f(3);
new f((x: number) => 'string');
