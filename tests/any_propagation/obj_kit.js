{
    declare function f<T>({...{x: T}}, T => number): T;

    f((42: any), (value) => { return 42 }); // no error
}

{
    declare function f<T>($ReadOnly<{x: T}>, T => number): T;

    f((42: any), (value) => { return 42 }); // no error
}
