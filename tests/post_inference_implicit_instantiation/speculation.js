//@flow

declare function f<T> (x: number => T): T;
declare function f<T>(x: T => void): T;

f((x: string) => {}); // Ok: It will error under Pierce, but we also consider upper bounds here.
