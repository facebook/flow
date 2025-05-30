// @flow

declare var _: any;

function test1() {
    type T1 = 'a1' | 'b1' | 'c1';
    type T2 = 'a2' | 'b2' | 'b3';

    declare var t1: T1;
    declare function t2(T2): void;
    t2(t1); // error (function)
    t1 as T2; // error (cast)
}

function test2() {
    const obj = { a: 'a', b: 'b', c: 'c' };

    type T1 = 'a1' | 'b1' | 'c1';
    type T2 = $Values<typeof obj>;

    _ as T1 as T2; // okay due to quick_subtype
    _ as T2 as T1; // error fails quick_subtype and slow path
}

function test3() {
    const obj = { a: 'a', b: 'b', c: 'c' } as const;

    type T1 = 'a' | 'b' | 'c';
    type T2 = $Values<typeof obj>;

    _ as T1 as T2; // okay keys match through quick_subtype
    _ as T2 as T1; // okay keys match through quick_subtype
}

function test4() {
    const obj = { a: 1, b: 2, c: 3 };

    type T1 = 'a' | 'b' | 'c';
    type T2 = $Values<typeof obj>;

    _ as T1 as T2; // error -- expanded pair-wise due to mismatching tags
    _ as T2 as T1; // error -- folded into a single error
}

function test5() {
    type T1 = 'a' | 'b' | 'c' | 'a1' | 'b1' | 'c1' | 'd1' | 'e1' | 'f1' | 'g1' | 'h1';
    type T2 = 'a' | 'b' | 'c' | 'a2' | 'b2' | 'c2';

    _ as T1 as T2; // error
    _ as T2 as T1; // error
}
