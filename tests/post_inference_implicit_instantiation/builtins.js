// @flow

// This test requires builtins to be properly loaded in the post-inference pass
function foo<X>(x: React$Element<X>): void {};
declare var x: React$Node;
// $FlowExpectedError
foo(x);
