{
    declare function f<T>({...{x: T}}, T => number): T;

    f((42: any), (value) => { return 42 }); // no error
}

{
    declare function f<T>(Readonly<{x: T}>, T => number): T;

    f((42: any), (value) => { return 42 }); // no error
}
