//@flow
component Foo() renders null { return null }
component Bar() renders React$Node { return null }
component Baz() renders React$Element<typeof Foo> { return (null: any) }
component Qux() renders (React$Element<typeof Foo> | React$Element<typeof Bar>) { return (null: any) }

declare const rendersFoo: renders React$Element<typeof Foo>;
declare const rendersBaz: renders React$Element<typeof Baz>;
declare const rendersNode: renders React$Node;
declare const rendersFooOrBar: renders (React$Element<typeof Foo> | React$Element<typeof Bar>);
declare const rendersFooOrRendersBar: renders React$Element<typeof Foo> | renders React$Element<typeof Bar>;
declare const rendersQux: renders React$Element<typeof Qux>;
declare const rendersBazOrBaz: renders (React$Element<typeof Baz> | React$Element<typeof Baz>);

/* Renders ~> Union */
{
  (rendersNode: React$Node); // OK
  (rendersNode: renders React$Node); // OK
  (rendersFooOrBar: typeof rendersFooOrRendersBar); // ERROR
  (rendersFooOrRendersBar: typeof rendersFooOrBar); // OK
}

/* Nominal ~> Nominal */
{
  (rendersFoo: renders React$Element<typeof Bar>); // ERROR
  (rendersFoo: renders React$Element<typeof Bar>); // ERROR
  (rendersBaz: typeof rendersFoo); // OK
  (rendersFoo: renders renders React$Element<typeof Bar>); // ERROR
  (rendersFoo: renders renders React$Element<typeof Bar>); // ERROR
  (rendersBaz: renders typeof rendersFoo); // OK
}

/* Nominal ~> Structural */
{
  (rendersFoo: typeof rendersFooOrBar); // OK
  (rendersFoo: renders (React$Element<typeof Foo> | null | React$Element<typeof Bar>)); // OK
  (rendersFoo: renders React$MixedElement); // OK
  (rendersQux: renders (React$Element<typeof Foo> | React$Element<typeof Bar>)); // OK
  (rendersQux: renders (React$Element<typeof Foo> | React$Element<typeof Baz>)); // ERROR
  component A0() renders null { return null; }
  component A1() renders (React$Element<typeof A0> | null) { return null; }
  declare const x: renders React$Element<typeof A1>; // OK
  (x: renders null); // OK
  (x: renders (null | null)); // OK
  (x: renders (null | React$Element<typeof A1>)); // OK
}

/* Structural ~> Nominal */
{
  (rendersBazOrBaz: renders React$Element<typeof Foo>); // OK
  (rendersBazOrBaz: renders React$Element<typeof Bar>); // ERROR
}

/* Structural ~> Structural */
{
  (rendersFooOrBar: typeof rendersFooOrBar); // OK
  (rendersBazOrBaz: renders (React$Element<typeof Foo> | React$Element<typeof Baz>)); // OK
  (rendersFooOrBar: renders (React$Element<typeof Foo> | React$Element<typeof Bar>)); // OK
  (rendersFooOrBar: renders (React$Element<typeof Foo> | React$Element<typeof Baz>)); // ERROR
}

/* Enter Structural Render Types */
{
  (null: renders null); // OK
  (Foo: renders React$Element<typeof Foo>); // ERROR
  (3: renders (null | number)); // OK
  declare const x: React$Element<()=>React$Node>;
  (x: renders typeof x); // OK
  declare const mixedElement: React$MixedElement;
  // The test below ensures repositioning does not hit unsoundness in speculation
  (mixedElement: renders (React$Element<typeof Foo> | React$Element<typeof Bar>)); // ERROR
}

/* Exit Structural Render Types */
{
  (rendersFooOrBar: React$MixedElement); // OK
  (rendersFooOrBar: empty); // ERROR
  (rendersFooOrBar: React$Node); // OK
  (rendersFooOrBar: React$Node); // OK
  (rendersNode: React$MixedElement); // ERROR
  (rendersNode: React$Node); // OK
  declare const rendersNullOrNull: renders (null | null);
  (rendersNullOrNull: null); // ERROR
  rendersFooOrBar.props; // ERROR
  rendersNode.props; // ERROR
}

/* Exit Nominal Render Types */
{
  (rendersBaz: React$MixedElement); // OK
  (rendersBaz: empty); // ERROR
  rendersBaz.props; // ERROR
}
