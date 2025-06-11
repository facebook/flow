declare const foo: {|
    a: string,
    b: string,
    c: string,
    d: string,
    e: string,
    f: string,
|};

foo as {||}; // error
