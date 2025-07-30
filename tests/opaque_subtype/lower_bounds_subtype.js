declare opaque type MiniReactElement: {...};
declare opaque type MiniReactNode super MiniReactElement | string;

function genericTest<N: MiniReactNode>(n: N) {
  n as MiniReactNode; // ok
}

function subtypingTest() {
  declare const e: MiniReactElement;
  e as MiniReactNode; // ok
}
