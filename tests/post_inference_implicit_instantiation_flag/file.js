//@flow
declare function f<T>(): T;
f(); // no error - flag is turned off
