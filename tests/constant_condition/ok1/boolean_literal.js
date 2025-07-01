{
    declare var x: boolean;
    if (x) {
        if (x) {} // ok, x is refined to `true` but the folder is not turned on
    }

    if (!x) {
        if (x) {} // error, x is refined to `false` but the folder is not turned on
    }
}
