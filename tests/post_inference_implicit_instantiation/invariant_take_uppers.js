//@flow

function f<T>(x: T => void): Array<T> { return [] }

const arr = f((x: number) => {}); // No error
(arr: Array<number>); // Ok
