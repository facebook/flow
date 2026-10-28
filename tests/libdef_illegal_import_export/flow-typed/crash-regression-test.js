// This is bad, but it shouldn't crash.
// mini-repro for https://github.com/meta/flow/issues/9262

import Unresolved from 'Unresolved'; // error: unsupported syntax
declare export class A mixins Unresolved {} // error: unresolved
