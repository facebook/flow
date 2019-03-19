//@flow

declare function foo(value: $Required<{ a?: number } | { b?: string }>): void;

foo({ a: 2 });
foo({ b: "bar" });
