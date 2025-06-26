{
    declare var x: boolean;
    if (x) {
        if (x) {} // ok, x is refined to `true` but the foler is not turned on
    }
}
