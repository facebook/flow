declare const x: mixed;

declare const foo: 1; // Is not def

const e = match (x) {
    {foo: true}: 0,
//   ^
    _: 0,
};
