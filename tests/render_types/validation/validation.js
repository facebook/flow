import * as React from 'react';

declare component Foo();
component Bar<T>() {return 0}
declare function Baz(): React$Node;
declare class Boz extends React.Component<{}> {}

type ReactElementAlias<T> = ExactReactElement_DEPRECATED<T>;
type RGood0 = renders 'svg'; // ok
type RGood1 = renders ExactReactElement_DEPRECATED<typeof Foo>; // ok
type RGood2 = renders ExactReactElement_DEPRECATED<typeof Foo | typeof Bar>; // ok
type RGood3 = renders ExactReactElement_DEPRECATED<typeof Foo> | ExactReactElement_DEPRECATED<typeof Bar>; // ok
type RGood4 = renders ReactElementAlias<typeof Foo>; // ok
type RGood5 = renders ReactElementAlias<typeof Foo | typeof Bar>; // ok
type RGood6 = renders ReactElementAlias<typeof Foo> | ReactElementAlias<typeof Bar>; // ok
type RGood7 = component () renders ExactReactElement_DEPRECATED<typeof Foo>; // ok
type RBad0 = renders ExactReactElement_DEPRECATED<'svg'>; // error
type RBad1 = renders ExactReactElement_DEPRECATED<typeof Baz>; // error
type RBad2 = renders ExactReactElement_DEPRECATED<typeof Boz>; // error
type RBad3 = renders 1; // error
type RBad4 = renders React$Node; // error
type RBad5 = renders Error; // error
type RBad6 = component () renders ExactReactElement_DEPRECATED<typeof Baz>; // error

component GoodComponentRenders() renders Foo {return <Foo />} // ok
component BadComponentRenders() renders ExactReactElement_DEPRECATED<typeof Baz> {return <Baz />} // error

component PermittedGenericRenders1<T: React$Node>(children: T) renders T { return children } // ok
component PermittedGenericRenders2<T: React$Node>(children: T) renders (T | T) { return children } // ok
component PermittedGenericRenders3<T: React$Node>(children: T) renders? T { return children } // ok
component BannedGenericRenders1<T: Error>(children: T) renders? T { return children } // error
component BannedGenericRenders2<T: React$Node>(children: T) renders? (T | GoodComponentRenders) { return children } // error
type AllowedGenericRenders<T: React$Node> = renders T; // ok

type BadSpecificRenders1 = renders (false | null | void); // error
type BadSpecificRenders2 = renders (Array<ExactReactElement_DEPRECATED<typeof Foo>>); // error
type BadSpecificRenders3 = renders ($ReadOnlyArray<ExactReactElement_DEPRECATED<typeof Foo>>); // error
type BadSpecificRenders4 = renders (Iterable<ExactReactElement_DEPRECATED<typeof Foo>>); // error
type BadSpecificRenders5 = renders (React.ChildrenArray<ExactReactElement_DEPRECATED<typeof Foo>>); // error
type BadSpecificRenders6 = renders (React.ChildrenArray<ExactReactElement_DEPRECATED<typeof Foo> | null>); // error

type BadUnion = renders ExactReactElement_DEPRECATED<typeof Bar | typeof Baz>; // error

type BadStructuralComponent = renders ExactReactElement_DEPRECATED<component() renders number>; // error
type GoodStructuralComponent = renders ExactReactElement_DEPRECATED<component() renders Foo>; // ok

// Showing why generic renders should be allowed everywhere
// If it's not allowed everywhere, then we have to make the hook return annotation
// to be an exact react element.
{
  hook useTransparent<T: React.Node>(n: T): renders T { // ok
    return <>{n}</>;
  }
  component GenericRenders<T: React.Node>(n: T) renders T {
    return useTransparent(n); // ok
  }
}
