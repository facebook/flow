//@flow

declare function f<T> (x: number => T): T;
declare function f<T>(x: T => void): T;

f((x: string) => {});
