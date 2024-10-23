function promotion_on_cyclic() {
  declare component Foo();
  declare component Bar() renders Bar;

  type T = ExactReactElement_DEPRECATED<typeof Bar>;
  declare const recursiveElement: T;
  recursiveElement as renders Foo; // TODO: no error, hit the same TypeAppT again
}
