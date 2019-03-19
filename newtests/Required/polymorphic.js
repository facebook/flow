declare function foo<T: $Required<{ a?: number }>>(val: T): void;

foo({ a: undefined }); // error
foo({ a: 1 });

declare function bar<T: { a?: number }>(val: $Required<T>): void;

bar({ a: undefined }); // error
bar({ a: 1 });
