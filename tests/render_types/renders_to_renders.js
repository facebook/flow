//@flow
component Foo() renders null { // invalid-render
  return null;
}
component Bar() renders React$Node { // invalid-render
  return null;
}
component Baz() renders ExactReactElement_DEPRECATED<typeof Foo> {
  return null as any;
}
component Qux() renders (
  | ExactReactElement_DEPRECATED<typeof Foo>
  | ExactReactElement_DEPRECATED<typeof Bar>
) {
  return null as any;
}

declare const rendersFoo: renders ExactReactElement_DEPRECATED<typeof Foo>;
declare const rendersBaz: renders ExactReactElement_DEPRECATED<typeof Baz>;
declare const rendersNode: renders React$Node; // invalid-render
declare const rendersFooOrBar: renders (
  | ExactReactElement_DEPRECATED<typeof Foo>
  | ExactReactElement_DEPRECATED<typeof Bar>
);
declare const rendersFooOrRendersBar:
  | renders ExactReactElement_DEPRECATED<typeof Foo>
  | renders ExactReactElement_DEPRECATED<typeof Bar>;
declare const rendersQux: renders ExactReactElement_DEPRECATED<typeof Qux>;
declare const rendersBazOrBaz: renders (
  | ExactReactElement_DEPRECATED<typeof Baz>
  | ExactReactElement_DEPRECATED<typeof Baz>
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
  rendersFoo as renders ExactReactElement_DEPRECATED<typeof Bar>; // ERROR
  rendersFoo as renders ExactReactElement_DEPRECATED<typeof Bar>; // ERROR
  rendersBaz as typeof rendersFoo; // OK
  rendersFoo as renders (renders ExactReactElement_DEPRECATED<typeof Bar>); // ERROR
  rendersFoo as renders (renders ExactReactElement_DEPRECATED<typeof Bar>); // ERROR
  rendersBaz as renders typeof rendersFoo; // type checks, but invalid-render
}

/* Nominal ~> Structural */
{
  rendersFoo as typeof rendersFooOrBar; // OK
  rendersFoo as renders (
    | ExactReactElement_DEPRECATED<typeof Foo>
    | null  // invalid-render
    | ExactReactElement_DEPRECATED<typeof Bar>
  ); // OK
  rendersFoo as renders React$MixedElement; // type checks, but invalid-render
  rendersQux as renders (ExactReactElement_DEPRECATED<typeof Foo> | ExactReactElement_DEPRECATED<typeof Bar>); // OK
  rendersQux as renders (ExactReactElement_DEPRECATED<typeof Foo> | ExactReactElement_DEPRECATED<typeof Baz>); // ERROR
  component A0() renders null { // invalid-render
    return null;
  }
  component A1() renders (ExactReactElement_DEPRECATED<typeof A0> | null) { // invalide-render
    return null;
  }
  declare const x: renders ExactReactElement_DEPRECATED<typeof A1>; // OK
  x as renders null; // type checks, but invalid-render
  x as renders (null | null);  // type checks, but invalid-render
  x as renders (null | ExactReactElement_DEPRECATED<typeof A1>);  // type checks, but invalid-render
}

/* Structural ~> Nominal */
{
  rendersBazOrBaz as renders ExactReactElement_DEPRECATED<typeof Foo>; // OK
  rendersBazOrBaz as renders ExactReactElement_DEPRECATED<typeof Bar>; // ERROR
}

/* Structural ~> Structural */
{
  rendersFooOrBar as typeof rendersFooOrBar; // OK
  rendersBazOrBaz as renders (
    | ExactReactElement_DEPRECATED<typeof Foo>
    | ExactReactElement_DEPRECATED<typeof Baz>
  ); // OK
  rendersFooOrBar as renders (
    | ExactReactElement_DEPRECATED<typeof Foo>
    | ExactReactElement_DEPRECATED<typeof Bar>
  ); // OK
  rendersFooOrBar as renders (
    | ExactReactElement_DEPRECATED<typeof Foo>
    | ExactReactElement_DEPRECATED<typeof Baz>
  ); // ERROR
}

/* Enter Structural Render Types */
{
  null as renders null; // type checks, but invalid-render
  Foo as renders ExactReactElement_DEPRECATED<typeof Foo>; // ERROR
  3 as renders (null | number); // type checks, but invalid-render
  declare const x: ExactReactElement_DEPRECATED<() => React$Node>;
  x as renders typeof x; // type checks, but invalid-render
  declare const mixedElement: React$MixedElement;
  // The test below ensures repositioning does not hit unsoundness in speculation
  mixedElement as renders (
    | ExactReactElement_DEPRECATED<typeof Foo>
    | ExactReactElement_DEPRECATED<typeof Bar>
  ); // ERROR
}

/* Exit Structural Render Types */
{
  rendersFooOrBar as React$MixedElement; // OK
  rendersFooOrBar as empty; // ERROR
  rendersFooOrBar as React$Node; // OK
  rendersFooOrBar as React$Node; // OK
  rendersNode as React$MixedElement; // ERROR
  rendersNode as React$Node; // OK
  declare const rendersNullOrNull: renders (null | null); // invalid-render
  rendersNullOrNull as null; // ERROR
  rendersFooOrBar.props; // ERROR
  rendersNode.props; // ERROR
}

/* Exit Nominal Render Types */
{
  rendersBaz as React$MixedElement; // OK
  rendersBaz as empty; // ERROR
  rendersBaz.props; // ERROR
}
