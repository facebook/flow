declare const x: mixed;

declare const foo: 1;

const e = match (x) {
    foo => 0,
//  ^
    [foo] => 0,
//   ^
    _ => 0,
};
