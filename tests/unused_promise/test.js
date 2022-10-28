// @flow

declare function foo(): Promise<void>;

class MyPromise extends Promise<void> {}

declare function bar(): MyPromise;

function baz() {
    foo(); // no error, not in async context
}

async function qux() {
    foo(); // error

    bar(); // error

    let x = foo();
    let y;
    y = x; // no error, expression is assignment
}
