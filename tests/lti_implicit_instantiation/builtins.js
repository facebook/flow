// This test requires builtins to be properly loaded in the post-inference pass
function foo<X>(x: ExactReactElement_DEPRECATED<X>): void {};
declare var x: React.Node;
// $FlowExpectedError[incompatible-type]
foo(x);
