// @flow

unbound = 1; // Error

function actuallyBound() {
    let unbound;
    unbound = 3; // No error, should not be confused with the real unbound name.
}

(unbound: UnboundType); // Error
(unbound: UnboundType); // Error
(unbound: UnboundType); // Error

unboundFunction(); // Error
unboundFunction(); // No error, only error once
unboundFunction(); // No error, only error once

if (true) {
    unboundFunction2(); // Error
}
if (true) {
    unboundFunction2(); // Error
}
if (true) {
    unboundFunction2(); // Error
}
