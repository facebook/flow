// @flow

enum A {}
for (const x of A) {} // Error: not iterable, but there should be no enforcement errors
