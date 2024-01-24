//@flow
component Foo() renders null { // invalid-render
  return null;
}
component Bar() renders React$Node { // invalid-render
  return null;
}
component Baz() renders React$Element<typeof Foo> {
  return null as any;
}
component Qux() renders (
  | React$Element<typeof Foo>
  | React$Element<typeof Bar>
) {
  return null as any;
}

declare const rendersFoo: renders React$Element<typeof Foo>;
declare const rendersBaz: renders React$Element<typeof Baz>;
declare const rendersNode: renders React$Node; // invalid-render
declare const rendersFooOrBar: renders (
  | React$Element<typeof Foo>
  | React$Element<typeof Bar>
);
declare const rendersFooOrRendersBar:
  | renders React$Element<typeof Foo>
  | renders React$Element<typeof Bar>;
declare const rendersQux: renders React$Element<typeof Qux>;
declare const rendersBazOrBaz: renders (
  | React$Element<typeof Baz>
  | React$Element<typeof Baz>
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
  rendersFoo as renders React$Element<typeof Bar>; // ERROR
  rendersFoo as renders React$Element<typeof Bar>; // ERROR
  rendersBaz as typeof rendersFoo; // OK
  rendersFoo as renders (renders React$Element<typeof Bar>); // ERROR
  rendersFoo as renders (renders React$Element<typeof Bar>); // ERROR
  rendersBaz as renders typeof rendersFoo; // type checks, but invalid-render
}

/* Nominal ~> Structural */
{
  rendersFoo as typeof rendersFooOrBar; // OK
  rendersFoo as renders (
    | React$Element<typeof Foo>
    | null  // invalid-render
    | React$Element<typeof Bar>
  ); // OK
  rendersFoo as renders React$MixedElement; // type checks, but invalid-render
  rendersQux as renders (React$Element<typeof Foo> | React$Element<typeof Bar>); // OK
  rendersQux as renders (React$Element<typeof Foo> | React$Element<typeof Baz>); // ERROR
  component A0() renders null { // invalid-render
    return null;
  }
  component A1() renders (React$Element<typeof A0> | null) { // invalide-render
    return null;
  }
  declare const x: renders React$Element<typeof A1>; // OK
  x as renders null; // type checks, but invalid-render
  x as renders (null | null);  // type checks, but invalid-render
  x as renders (null | React$Element<typeof A1>);  // type checks, but invalid-render
}

/* Structural ~> Nominal */
{
  rendersBazOrBaz as renders React$Element<typeof Foo>; // OK
  rendersBazOrBaz as renders React$Element<typeof Bar>; // ERROR
}

/* Structural ~> Structural */
{
  rendersFooOrBar as typeof rendersFooOrBar; // OK
  rendersBazOrBaz as renders (
    | React$Element<typeof Foo>
    | React$Element<typeof Baz>
  ); // OK
  rendersFooOrBar as renders (
    | React$Element<typeof Foo>
    | React$Element<typeof Bar>
  ); // OK
  rendersFooOrBar as renders (
    | React$Element<typeof Foo>
    | React$Element<typeof Baz>
  ); // ERROR
}

/* Enter Structural Render Types */
{
  null as renders null; // type checks, but invalid-render
  Foo as renders React$Element<typeof Foo>; // ERROR
  3 as renders (null | number); // type checks, but invalid-render
  declare const x: React$Element<() => React$Node>;
  x as renders typeof x; // type checks, but invalid-render
  declare const mixedElement: React$MixedElement;
  // The test below ensures repositioning does not hit unsoundness in speculation
  mixedElement as renders (
    | React$Element<typeof Foo>
    | React$Element<typeof Bar>
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
