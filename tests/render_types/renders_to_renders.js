//@flow
component Foo() renders $Renders<null> { return null }
component Bar() renders $Renders<React$Node> { return null }
component Baz() renders $Renders<Foo> { return (null: any) }
component Qux() renders $Renders<Foo | Bar> { return (null: any) }

declare const rendersFoo: $Renders<Foo>;
declare const rendersElemFoo: $Renders<React$Element<Foo>>;
declare const rendersBaz: $Renders<Baz>;
declare const rendersNode: $Renders<React$Node>;
declare const rendersFooOrBar: $Renders<Foo | Bar>;
declare const rendersFooOrRendersBar: $Renders<Foo> | $Renders<Bar>;
declare const rendersQux: $Renders<Qux>;
declare const rendersBazOrBaz: $Renders<Baz | Baz>;

/* Renders ~> Union */
{
  (rendersNode: React$Node); // OK
  (rendersNode: $Renders<React$Node>); // OK
  (rendersFooOrBar: typeof rendersFooOrRendersBar); // ERROR
  (rendersFooOrRendersBar: typeof rendersFooOrBar); // OK
}

/* Nominal ~> Nominal */
{
  (rendersFoo: typeof rendersElemFoo); // OK
  (rendersElemFoo: typeof rendersFoo); // OK
  (rendersFoo: $Renders<Bar>); // ERROR
  (rendersFoo: $Renders<React$Element<Bar>>); // ERROR
  (rendersBaz: typeof rendersFoo); // OK
  (rendersBaz: typeof rendersElemFoo); // OK
  (rendersFoo: $Renders<typeof rendersElemFoo>); // OK
  (rendersElemFoo: $Renders<typeof rendersFoo>); // OK
  (rendersFoo: $Renders<$Renders<Bar>>); // ERROR
  (rendersFoo: $Renders<$Renders<React$Element<Bar>>>); // ERROR
  (rendersBaz: $Renders<typeof rendersFoo>); // OK
  (rendersBaz: $Renders<typeof rendersElemFoo>); // OK
}

/* Nominal ~> Structural */
{
  (rendersFoo: typeof rendersFooOrBar); // OK
  (rendersFoo: $Renders<Foo | null | Bar>); // OK
  (rendersFoo: $Renders<React$MixedElement>); // OK
  (rendersQux: $Renders<Foo | Bar>); // OK
  (rendersQux: $Renders<Foo | Baz>); // ERROR
  component A0() renders $Renders<null> { return null; }
  component A1() renders $Renders<A0 | null> { return null; }
  declare const x: $Renders<A1>; // OK
  (x: $Renders<null>); // OK
  (x: $Renders<null | null>); // OK
  (x: $Renders<null | A1>); // OK
}

/* Structural ~> Nominal */
{
  (rendersBazOrBaz: $Renders<Foo>); // OK
  (rendersBazOrBaz: $Renders<Bar>); // ERROR
}

/* Structural ~> Structural */
{
  (rendersFooOrBar: typeof rendersFooOrBar); // OK
  (rendersBazOrBaz: $Renders<Foo | Baz>); // OK
  (rendersFooOrBar: $Renders<Foo | Bar>); // OK
  (rendersFooOrBar: $Renders<Foo | Baz>); // ERROR
}

/* Enter Structural Render Types */
{
  (null: $Renders<null>); // OK
  (Foo: $Renders<Foo>); // ERROR
  (3: $Renders<null | number>); // OK
  declare const x: React$Element<()=>React$Node>;
  (x: $Renders<typeof x>); // OK
}

/* Exit Structural Render Types */
{
  (rendersFooOrBar: React$MixedElement); // OK
  (rendersFooOrBar: empty); // ERROR
  (rendersFooOrBar: React$Node); // OK
  (rendersFooOrBar: React$Node); // OK
  (rendersNode: React$MixedElement); // ERROR
  (rendersNode: React$Node); // OK
  declare const rendersNullOrNull: $Renders<null | null>;
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
