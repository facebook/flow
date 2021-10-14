// @flow

// Can't construct
new JSON(); // ERROR

// Can unbind
const parse = JSON.parse; // OK
const stringify = JSON.stringify; // OK

// Read-only
JSON.parse = 1; // ERROR
