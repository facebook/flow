class Foo<T> {}
declare const FooClass: typeof Foo<string>;

declare function bar<T>(): T;
declare const barF: typeof bar<string>;
