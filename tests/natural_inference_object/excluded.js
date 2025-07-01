const foo = {};
foo as {bar?: string}; // no error, excluded from enforcement.
