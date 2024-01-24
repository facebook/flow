export class FooClassExported<T> {}
declare export const FooClassStringExported: typeof FooClassExported<string>;

declare function bar<T>(): T;
declare export const barFExported: typeof bar<string>;

declare export function barGenericExported<X>(cb: typeof bar<X>): X;

export class EscapedGenericExported<T> {
  static x: T;
}
