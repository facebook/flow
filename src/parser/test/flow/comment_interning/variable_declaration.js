function leadingOnly() {
    /* 1.1 Leading */ var x = 1;
}

function trailingOnly() {
    let x = 1; /* 2.1 Trailing */
}

function leadingAndTrailing() {
    /* 3.1 Leading */ const x = 1; /* 3.2 Trailing */
}

function innerComments() {
    /* 4.1 L decl */ var /* 4.2 L id */ x /* 4.3 T id */ = /* 4.4 L num */ 1 /* 4.5 T num */; /* 4.6 T decl */
}

function forInitComments() {
    for (/* 5.1 Leading */ var x = 1; /* 5.2 Not trailing */ x < 10; x++) {}
}

function forOfComments() {
    for (/* 6.1 Leading */ let x /* 6.2 Not trailing */ of []) {}
}

function forInComments() {
    for (/* 7.1 Leading */ let x /* 7.2 Not trailing */ in {}) {}
}
