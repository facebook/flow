// @flow

import * as React from "react";

declare function Foo(props: { x: number }): React.Node;

(<Foo x />);
//    ^

(<Foo x="" />);
//    ^

(<Foo x={""} />);
//    ^

(<Foo x={0} />);
//    ^

(<Foo key={0} />);
//    ^

(<Foo ref={0} />);
//    ^

declare function Poly<T>(props: { x: T }): React.Node;

(<Poly<_> x={0} />);
//     ^

// An element name is a reference to whatever is in scope under that name, so it
// is framed as that declaration rather than as the bare type it evaluates to.
// Both ends of a non-self-closing element name the same one.

(<Foo x={0} />);
//^

(<Foo x={0}></Foo>);
//            ^

declare component Comp(a: number);

(<Comp a={0} />);
//^

// An intrinsic is a name, but not one anything binds, so it keeps printing its
// type.

(<div id="" />);
//^

// A member-expression name frames both halves: the receiver as the declaration
// it refers to, the property as a member of it.

declare var NS: { Sub: typeof Foo };

(<NS.Sub x={0} />);
//^

(<NS.Sub x={0} />);
//   ^
