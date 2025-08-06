declare opaque type MiniReactElement: {...};
declare opaque type MiniReactNode super MiniReactElement | string;

function genericTest<N: MiniReactNode>(n: N) {
  n as MiniReactNode; // ok
}

function subtypingTest() {
  declare const e: MiniReactElement;
  e as MiniReactNode; // ok
}

function renderTypeTest() {
  declare component C();

  declare const rendersOne: renders C;
  declare const rendersMaybeOne: renders? C;
  declare const rendersMany: renders* C;

  rendersOne as React.Node; // ok
  rendersMaybeOne as React.Node; // ok
  rendersMany as React.Node; // ok
}
