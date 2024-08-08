function promotion_on_cyclic_1() {
  declare component Foo();

  type T = React.Element<React.AbstractComponent<empty, mixed, T>>;
  declare const recursiveElement: T;
  recursiveElement as renders Foo; // TODO: no error, hit the same TypeAppT again
}

// The nature of this test is the same as the one above,
// but writing it in this way to show that the bug can be triggered on legit code.
function promotion_on_cyclic_2() {
  declare component Foo();
  declare component Bar() renders Bar;

  type T = React.Element<typeof Bar>;
  declare const recursiveElement: T;
  recursiveElement as renders Foo; // TODO: no error, hit the same TypeAppT again
}
