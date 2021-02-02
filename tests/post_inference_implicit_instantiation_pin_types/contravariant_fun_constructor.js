//@flow
declare function f<T>(T => mixed): T => mixed;
new f((x: number) => 'string');


declare function g<T>(T => mixed, T => mixed): T => mixed;
new g((x: string) => 3, (x: number) => 'string');
