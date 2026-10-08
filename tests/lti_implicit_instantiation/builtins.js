// This test requires builtins to be properly loaded in the post-inference pass
function foo<X extends React.ElementType>(x: React.ComponentProps<X>): void {};
declare const x: React.Node;
// $FlowExpectedError[incompatible-type]
foo(x);
