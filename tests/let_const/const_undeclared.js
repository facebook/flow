/*
This error is actually a syntax error. Files with syntax errors don't go
through the type checkers, so we can't mix type error tests with this
file.
*/

function requiresInitializer() {
  const foo; // error
}
