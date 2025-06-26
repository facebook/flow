{
    declare var x: boolean;
    if (x) {
        if (x) {} // error, x is refined to `true`
    }
}
