declare component Foo(n: number);

declare var config: React.ElementConfig<typeof Foo>;
config as {n: number | string}; // error: n is not read-only in {n: number | string}

declare var props: React.ElementProps<typeof Foo>;
props as {n: number | string}; // error: n is not read-only in {n: number | string}
