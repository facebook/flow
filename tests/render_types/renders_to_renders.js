//@flow
component FooBad() renders null { /* invalid-render */ return null; }
component Foo() { return null; }

component BarBad() renders React$Node { /* invalid-render */ return null; }
component Bar() {  return null; }

component Baz() renders Foo {
  return null as any;
}
component Qux() renders (
  | Foo
  | Bar
) {
  return null as any;
}

declare const rendersFoo: renders Foo;
declare const rendersBaz: renders Baz;
declare const rendersNode: renders React$Node; // invalid-render
declare const rendersFooOrBar: renders (
  | Foo
  | Bar
);
declare const rendersFooOrRendersBar:
  | renders Foo
  | renders Bar;
declare const rendersQux: renders Qux;
declare const rendersBazOrBaz: renders (
  | Baz
  | Baz
);

/* Renders ~> Union */
{
  rendersNode as React$Node; // OK
  rendersNode as renders React$Node; // type checks, but invalid-render
  rendersFooOrBar as typeof rendersFooOrRendersBar; // ERROR
  rendersFooOrRendersBar as typeof rendersFooOrBar; // OK
}

/* Nominal ~> Nominal */
{
  rendersFoo as renders Bar; // ERROR
  rendersFoo as renders Bar; // ERROR
  rendersBaz as typeof rendersFoo; // OK
  rendersFoo as renders (renders Bar); // invalid-renders turns RHS any
  rendersFoo as renders (renders Bar); // invalid-renders turns RHS any
  rendersBaz as renders typeof rendersFoo; // type checks, but invalid-render
}

/* Nominal ~> Structural */
{
  rendersFoo as typeof rendersFooOrBar; // OK
  rendersFoo as renders (
    | Foo
    | null  // invalid-render
    | Bar
  ); // OK
  rendersFoo as renders React.MixedElement; // type checks, but invalid-render
  rendersQux as renders (Foo | Bar); // OK
  rendersQux as renders (Foo | Baz); // ERROR
  component A0() renders null { // invalid-render
    return null;
  }
  component A1() renders (A0 | null) { // invalid-render
    return null;
  }
  declare const x: renders A1; // OK
  x as renders null; // type checks, but invalid-render
  x as renders (null | null);  // type checks, but invalid-render
  x as renders (null | A1);  // type checks, but invalid-render
}

/* Structural ~> Nominal */
{
  rendersBazOrBaz as renders Foo; // OK
  rendersBazOrBaz as renders Bar; // ERROR
}

/* Structural ~> Structural */
{
  rendersFooOrBar as typeof rendersFooOrBar; // OK
  rendersBazOrBaz as renders (
    | Foo
    | Baz
  ); // OK
  rendersFooOrBar as renders (
    | ExactReactElement_DEPRECATED<typeof Foo>
    | Bar
  ); // OK
  rendersFooOrBar as renders (
    | Foo
    | Baz
  ); // ERROR
}

/* Enter Structural Render Types */
{
  null as renders null; // type checks, but invalid-render
  Foo as renders Foo; // ERROR
  3 as renders (null | number); // type checks, but invalid-render
  declare const x: ExactReactElement_DEPRECATED<() => React$Node>;
  x as renders typeof x; // type checks, but invalid-render
  declare const mixedElement: React.MixedElement;
  // The test below ensures repositioning does not hit unsoundness in speculation
  mixedElement as renders (
    | Foo
    | Bar
  ); // ERROR
  declare const anyElement: ExactReactElement_DEPRECATED<any>;
  anyElement as renders Foo; // ok
}

/* Exit Structural Render Types */
{
  rendersFooOrBar as React.MixedElement; // OK
  rendersFooOrBar as empty; // ERROR
  rendersFooOrBar as React$Node; // OK
  rendersFooOrBar as React$Node; // OK
  rendersNode as React.MixedElement; // invalid-render of rendersNode makes LHS any
  rendersNode as React$Node; // OK
  declare const rendersNullOrNull: renders (null | null); // invalid-render
  rendersNullOrNull as null; // invalid-render of rendersNullOrNull makes LHS any
  rendersFooOrBar.props; // ERROR
  rendersNode.props; // ERROR
}

/* Exit Nominal Render Types */
{
  rendersBaz as React.MixedElement; // OK
  rendersBaz as empty; // ERROR
  rendersBaz.props; // ERROR
}
