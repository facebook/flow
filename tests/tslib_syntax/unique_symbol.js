declare const x: unique symbol;

// Should be usable as symbol
x as symbol; // OK

// Should not be assignable to string
x as empty; // ERROR
