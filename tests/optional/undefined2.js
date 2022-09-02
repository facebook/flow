var x;

function foo(bar?: {duck: mixed}) {
    x = bar;
}

function bar() {
    return x.duck;
}
