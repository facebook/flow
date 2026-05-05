var x;

function foo(bar?: {duck: unknown} = undefined) {
    x = bar;
}

function bar() {
    return x.duck;
}
