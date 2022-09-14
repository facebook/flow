// @flow

function foo() {}
foo.bar = (baz) => {}; // Error: function statics assignments do not have hints.
