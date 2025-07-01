{
    declare var x: ?{};
    if (!x) {
        if (x) {} // ok, x is refined to `null | void`, but directory is turned off for null and void
    }

    let y: Array<number>;
    if (y) {} // error, y is undefined, but directory is turned off for null and void
}
