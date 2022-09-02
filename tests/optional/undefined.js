var x;

function foo(bar?: {duck: mixed} = undefined) {
    x = bar;
}

function bar() {
    return x.duck;
}
