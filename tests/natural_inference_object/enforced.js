const foo = {};
foo as {bar?: string}; // error

const bar = {baz: 1};
bar as {baz: string | number}; // error
