function bar(x: mixed) { return x; }
function foo() {
    return function bound(): mixed {
        return bar(bound);
    };
}
