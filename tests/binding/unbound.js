// @flow

unbound = 1; // Error

function actuallyBound() {
    let unbound;
    unbound = 3; // No error, should not be confused with the real unbound name.
}

(unbound: UnboundType); // Error
(unbound: UnboundType); // No error, only error once
(unbound: UnboundType); // No error, only error once

unboundFunction(); // Error
unboundFunction(); // No error, only error once
unboundFunction(); // No error, only error once

if (true) {
    unboundFunction2(); // Error
}
if (true) {
    unboundFunction2(); // No error, only error once
}
if (true) {
    unboundFunction2(); // No error, only error once
}
