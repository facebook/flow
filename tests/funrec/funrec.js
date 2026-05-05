function bar(x: unknown) { return x; }
function foo() {
    return function bound(): unknown {
        return bar(bound);
    };
}
