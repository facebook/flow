{
    declare var x: ?{};
    if (!x) {
        if (x) {} // error, x is refined to `null | void`
    }

    let y: Array<number>;
    if (y) {} // error, y is undefined
}
