const foo = {};
foo as {bar?: string}; // error. TODO: should show error with suggestions

const bar = {baz: 1};
bar as {baz: string | number}; // error

const bazArray = [{baz: 1}];
bazArray as Array<{baz: string | number}>; // error. TODO: should show error with suggestions
