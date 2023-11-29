component Foo(p: number, 'a' as b: string, ...rest: {...}) {
    p = 42; // error
    b = 'b'; // error
    rest = {}; // error

    let p; // error
    return null;
}
