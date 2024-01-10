export class FooClassExported<T> {}
declare export const FooClassStringExported: typeof FooClassExported<string>; // unsupported

declare function bar<T>(): T;
declare export const barFExported: typeof bar<string>; // unsupported

declare export function barGenericExported<X>(cb: typeof bar<X>): X; // unsupported
