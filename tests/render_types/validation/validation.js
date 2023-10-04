import * as React from 'react';

declare component Foo();
component Bar<T>() {return 0}
declare function Baz(): React$Node;
declare class Boz extends React$Component<{}> {}

type ReactElementAlias<T> = React$Element<T>;

type RGood1 = renders React$Element<typeof Foo>; // ok
type RGood2 = renders React$Element<typeof Foo | typeof Bar>; // ok
type RGood3 = renders React$Element<typeof Foo> | React$Element<typeof Bar>; // ok
type RGood4 = renders ReactElementAlias<typeof Foo>; // ok
type RGood5 = renders ReactElementAlias<typeof Foo | typeof Bar>; // ok
type RGood6 = renders ReactElementAlias<typeof Foo> | ReactElementAlias<typeof Bar>; // ok
type RGood7 = component () renders React$Element<typeof Foo>; // ok

type RBad1 = renders React$Element<typeof Baz>; // error
type RBad2 = renders React$Element<typeof Boz>; // error
type RBad3 = renders 1; // error
type RBad4 = renders React$Node; // error
type RBad5 = renders Error; // error
type RBad6 = component () renders React$Element<typeof Baz>; // error

component GoodComponentRenders() renders React$Element<typeof Foo> {return <Foo />} // ok
component BadComponentRenders() renders React$Element<typeof Baz> {return <Baz />} // error

component PermittedGenericRenders<T: React$Node>(children: T) renders T { return children } // ok
type BannedGenericRenders<T: React$Node> = renders T; // error

type BadSpecificRenders1 = renders (false | null | void); // error
type BadSpecificRenders2 = renders (Array<React$Element<typeof Foo>>); // error
type BadSpecificRenders3 = renders ($ReadOnlyArray<React$Element<typeof Foo>>); // error
type BadSpecificRenders4 = renders (Iterable<React$Element<typeof Foo>>); // error
type BadSpecificRenders5 = renders (React.ChildrenArray<React$Element<typeof Foo>>); // error
type BadSpecificRenders6 = renders (React.ChildrenArray<React$Element<typeof Foo> | null>); // error
