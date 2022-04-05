// @flow

unbound = 1; // Error

function actuallyBound() {
    let unbound;
    unbound = 3; // No error, should not be confused with the real unbound name.
}
