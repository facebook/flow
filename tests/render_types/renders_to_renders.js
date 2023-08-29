//@flow
component Foo() renders null { return null }
component Bar() renders React$Node { return null }
component Baz() renders Foo { return (null: any) }
component Qux() renders (Foo | Bar) { return (null: any) }

declare const rendersFoo: renders Foo;
declare const rendersElemFoo: renders React$Element<Foo>;
declare const rendersBaz: renders Baz;
declare const rendersNode: renders React$Node;
declare const rendersFooOrBar: renders (Foo | Bar);
declare const rendersFooOrRendersBar: renders Foo | renders Bar;
declare const rendersQux: renders Qux;
declare const rendersBazOrBaz: renders (Baz | Baz);

/* Renders ~> Union */
{
  (rendersNode: React$Node); // OK
  (rendersNode: renders React$Node); // OK
  (rendersFooOrBar: typeof rendersFooOrRendersBar); // ERROR
  (rendersFooOrRendersBar: typeof rendersFooOrBar); // OK
}

/* Nominal ~> Nominal */
{
  (rendersFoo: typeof rendersElemFoo); // OK
  (rendersElemFoo: typeof rendersFoo); // OK
  (rendersFoo: renders Bar); // ERROR
  (rendersFoo: renders React$Element<Bar>); // ERROR
  (rendersBaz: typeof rendersFoo); // OK
  (rendersBaz: typeof rendersElemFoo); // OK
  (rendersFoo: renders typeof rendersElemFoo); // OK
  (rendersElemFoo: renders typeof rendersFoo); // OK
  (rendersFoo: renders renders Bar); // ERROR
  (rendersFoo: renders renders React$Element<Bar>); // ERROR
  (rendersBaz: renders typeof rendersFoo); // OK
  (rendersBaz: renders typeof rendersElemFoo); // OK
}

/* Nominal ~> Structural */
{
  (rendersFoo: typeof rendersFooOrBar); // OK
  (rendersFoo: renders (Foo | null | Bar)); // OK
  (rendersFoo: renders React$MixedElement); // OK
  (rendersQux: renders (Foo | Bar)); // OK
  (rendersQux: renders (Foo | Baz)); // ERROR
  component A0() renders null { return null; }
  component A1() renders (A0 | null) { return null; }
  declare const x: renders A1; // OK
  (x: renders null); // OK
  (x: renders (null | null)); // OK
  (x: renders (null | A1)); // OK
}

/* Structural ~> Nominal */
{
  (rendersBazOrBaz: renders Foo); // OK
  (rendersBazOrBaz: renders Bar); // ERROR
}

/* Structural ~> Structural */
{
  (rendersFooOrBar: typeof rendersFooOrBar); // OK
  (rendersBazOrBaz: renders (Foo | Baz)); // OK
  (rendersFooOrBar: renders (Foo | Bar)); // OK
  (rendersFooOrBar: renders (Foo | Baz)); // ERROR
}

/* Enter Structural Render Types */
{
  (null: renders null); // OK
  (Foo: renders Foo); // ERROR
  (3: renders (null | number)); // OK
  declare const x: React$Element<()=>React$Node>;
  (x: renders typeof x); // OK
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
  rendersFooOrBar.props; // OK
  rendersNode.props; // ERROR
}

/* Exit Nominal Render Types */
{
  (rendersBaz: React$MixedElement); // OK
  (rendersBaz: empty); // ERROR
  rendersBaz.props; // OK
}
