/**
 * @flow
 */

function f(): number {
    throw new Error(); // OK to not return
}

function g(a: ?string) {
    if (a == null) {
        throw new Error();
    }
    return a*1; // a is not null
}
