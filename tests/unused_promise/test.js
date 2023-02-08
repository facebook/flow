declare function foo(): Promise<void>;

class MyPromise extends Promise<void> {}

declare function bar(): MyPromise;

async function qux() {
    foo(); // error

    bar(); // error

    let x = foo();
    let y;
    y = x; // no error, expression is assignment
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
