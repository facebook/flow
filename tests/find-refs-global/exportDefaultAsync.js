// @flow

import foo from './exportDefaultExpr';

// Strangely, the `async` causes this to be treated as an exported expression and it does *not*
// introduce the `foo` name into the top-level scope. However, we still want people to be able to
// start a find-refs request on the function name here.
export default async function foo() { foo(); }

// This refers to the imported foo
foo();
