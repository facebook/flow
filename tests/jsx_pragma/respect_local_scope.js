// @jsx Foo
const Bar = 123;
function Foo(x: string) {}
<Bar />; // error: number ~> string

{
  const Foo = (y: boolean) => {};
  <Bar />; // error: number ~> boolean
}

{
  function Foo(elem: number, props: { x: string }) {}
  // Second arg to jsx function should be props
  <Bar x={123} />; // error: number ~> string
}

{
  function Foo(elem: number, props: { x: string }) {}
  // Second arg to jsx function is null when there are no attributes
  <Bar />; // error: null ~> object type
}

{
  function Foo(elem: number, props: null, child1: number, child2: string) {}
  // Children are passed after the element and props
  <Bar>{true}{null}</Bar> // error
}

{
  function Foo(elem: number, props: {key: boolean, ref: number}) {}
  // React ignores certain props, but @jsx shouldn't
  <Bar key="hi" ref="bye" />; // error: string ~> boolean, string ~> number
}

{
  function Foo(elem: "bar") {}
  // jsx intrinsics should pass through a string
  <baz />; // error 'baz' ~> 'bar'
}

{
  function Foo(elem: number, props: {x: string}) {}
  // JSX element missing property should error
  <Bar y="hi" />; // error: missing x
}

{
  function Foo(elem: number) {}
  <Baz y="hi" />; // error: Cannot resolve Baz
}

{
  function Foo(elem: number, props: {| x: string |}) {}
  // Exact prop type without spread should work
  <Bar x="hi" />;
}

{
  function Foo(elem: number, props: null, child1: 'a', child2: 'b', child3: 'c') {}
  // Spread syntax in children should work
  <Bar>{...["a", "b", "c"]}</Bar>;
}

{
  function Foo(elem: number, props: {| x: string |}) {}
  const props = {x: "hi"};
  // Exact prop type with spread should work
  <Bar {...props} />;
}
