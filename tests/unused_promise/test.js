declare function foo(): Promise<void>;

class MyPromise extends Promise<void> {}

declare function bar(): MyPromise;

async function qux() {
    foo(); // error

    bar(); // error

    let x = foo();
    let y;
    y = x; // ok, expression is assignment
}

function valid() {
    foo().then(() => {}, () => {}); // ok
    foo().catch(() => {}); // ok
    foo().finally(() => {}); // ok
    foo().then(() => {}).then(() => {}, () => {}); // ok
    foo().then(() => {}).catch(() => {}); // ok
    foo().then(() => {}).finally(() => {}); // ok
}

async function validAsync() {
    await foo(); // ok
}

function invalid() {
    foo(); // error
    foo().then(() => {}); // error
    foo().then(() => {}).then(() => {}); // error
}

function logical(b: boolean) {
    b && foo(); // error
    b && foo() && b; // error
    b && b && foo(); // error
    foo() && foo(); // error
    foo().catch(() => {}) && foo(); // error
    foo() && foo().catch(() => {}); // error

    b && foo().catch(() => {}); // ok
    b && foo().catch(() => {}) && b; // ok
    b && b && foo().catch(() => {}); // ok
    foo().catch(() => {}) && foo().catch(() => {}); // ok
}

function ternary(b: boolean) {
    b ? foo() : 3; // error
    b ? 3 : foo(); // error

    b ? foo().catch(() => {}) : 3; // ok
    (b ? foo() : foo()).catch(() => {}); // ok
}
