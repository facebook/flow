//@flow

declare function f<T>(x: number): Map<string, T>;
declare function f<T>(x: string): Set<T>;

let num = f(3);
num.set('str', 3);

let str = f('str');
str.add('str')
