// This test requires builtins to be properly loaded in the post-inference pass
function foo<X>(x: React.ElementConfig<X>): void {};
declare const x: React.Node;
// $FlowExpectedError[incompatible-type]
foo(x);
