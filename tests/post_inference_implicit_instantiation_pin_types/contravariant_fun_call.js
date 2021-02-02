//@flow
declare function f<T>(T => mixed): T => mixed;
f((x: number) => 'string');


declare function g<T>(T => mixed, T => mixed): T => mixed;
g((x: string) => 3, (x: number) => 'string');
