function if_leading () {
    // not leading 1
    var pred = true;  // pre if leading 1
    /* pre if leading 2 */
    if /* pre cond leading 1 */ (pred) {
        pred = false;
    }
}

function if_trailing() {
    var pred = false;
    if (!pred) /* trailing 1 */ {
        pred = true;
    }
    // not trailing 1
}

function if_leading_and_trailing() {
    // not leading 2
    var pred = true;  // pre if leading 3
    /* pre if leading 4 */
    if /* pre cond leading 2 */ (pred) /* trailing 2 */ {
        pred = false;
    } /* not trailing 2 */ else /* pre if leading 5 */ if /* pre cond leading 3 */ (pred) /* trailing 3 */ {
        pref = true;
    }
    // not trailing 3
}
