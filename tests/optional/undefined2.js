var x;

function foo(bar?: {duck: unknown}) {
    x = bar;
}

function bar() {
    return x.duck;
}
