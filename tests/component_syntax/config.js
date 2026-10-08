declare component Foo(n: number);

declare export var config: React.ComponentProps<typeof Foo>;
config as {n: number | string}; // error: n is not read-only in {n: number | string}
