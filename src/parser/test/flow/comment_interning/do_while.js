function only_leading () {
    // 1.0. unreachable leading comment
    var i = 0;  // 1.1. leading comment
    // 1.2. another leading comment
    do {
        i += 1;
    } while (i < 3);
}

function leading_n_trailing() {
    var i = 0;
    // 2.1. leading comment
    do {
        i += 1;
    } while (i < 3) /* 2.2. trailing comment */;

    // 2.3. unreachable trailing comment
}

function only_trailing() {
    var i = 0;
    do {
        i += 1;
    } /* 3.0. pre-keyword trailing comment */ while /* 3.1. pre-cond trailing comment */ (i< 3) /* 3.2. past-cond trailing comment */;

    // 3.3. unreachable trailing comment
}
