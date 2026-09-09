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

// An attribute lexically inside a class body still frames as the component's
// (anonymous) props, not as a member of the enclosing class.
class C {
  render(): React.Node {
    return (<Foo x={0} />);
//               ^
  }
}

// A component declared with a rest props alias shows one level of props,
// both at the declaration site and at element names. Nested aliases stay
// written, and unions stay condensed.

type SpreadProps = { a: number, b: string };

declare component SpreadComp(...props: SpreadProps);
//                ^

(<SpreadComp a={0} b="" />);
//^

type Inner = { n: number };
type Outer = { x: Inner, y: number };

declare component OuterComp(...props: Outer);
//                ^

type A = { a: number };
type B = { b: string };

declare component UnionComp(...props: A | B);
//                ^

// A component with explicit params before the rest alias expands the alias
// alongside them.

declare component MixedComp(c: number, ...props: SpreadProps);
//                ^

// Utilities that reduce to plain props expand too.

declare component OmitComp(...props: Omit<SpreadProps, 'b'>);
//                ^

declare component PickComp(...props: Pick<SpreadProps, 'a'>);
//                ^

declare component ReadonlyComp(...props: Readonly<SpreadProps>);
//                ^

declare component PartialComp(...props: Partial<SpreadProps>);
//                ^

// A rest alias whose body itself spreads another alias flattens
// transitively instead of stopping halfway at `...{...Base, ...}`.

type Base = {a: number};
type PropsWithExtra = {...Base, extra: string};

declare component ChainComp(...props: PropsWithExtra);
//                ^

// Object resolution expands all sibling spreads, not only the first.

type SA = {a: number};
type SB = {b: number};
type SC = {c: number};
type SD = {d: number};
type FourProps = {...SA, ...SB, ...SC, ...SD};

declare component FourSpreads(...props: FourProps);
//                ^

// An expanded props object larger than the printer budget crops the tail.

type Keys = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z';

type BigProps = {
  m1: {[key in Keys]: {[key in Keys]: string}},
  m2: {[key in Keys]: {[key in Keys]: string}},
  m3: {[key in Keys]: {[key in Keys]: string}},
  tail1: number,
  tail2: number,
};

declare component BigComp(...props: BigProps);
//                ^

// A later prop overrides an earlier same-name prop from a spread, mirroring
// object spread semantics: one `b`, with the overriding type.

type OverBase = {a: number, b: number, keep: string};
type OverProps = {...OverBase, b: string, extra: boolean};

declare component OverComp(...props: OverProps);
//                ^
