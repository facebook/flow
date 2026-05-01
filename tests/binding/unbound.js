unbound = 1; // Error

function actuallyBound() {
    let unbound;
    unbound = 3; // No error, should not be confused with the real unbound name.
}

unbound as UnboundType; // Error
unbound as UnboundType; // Error
unbound as UnboundType; // Error

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
