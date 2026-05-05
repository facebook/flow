declare const x: unknown;

declare const Obj: {bar: 1};

const e = match (x) {
    Obj.foo => 0,
//  ^
    Obj.bar => 0,
//      ^
    _ => 0,
};
