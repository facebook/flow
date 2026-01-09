{
    declare function f<T>(a: {...{x: T}}, b: T => number): T;

    f((42: any), (value) => { return 42 }); // no error
}

{
    declare function f<T>(a: Readonly<{x: T}>, b: T => number): T;

    f((42: any), (value) => { return 42 }); // no error
}
