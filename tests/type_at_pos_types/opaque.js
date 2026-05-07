// @flow

export opaque type Foo = number;
//                 ^?

declare const foo: Foo;
//                  ^?
foo;
//^?

declare opaque type Bar: number;
//                   ^?

declare const bar: Bar;
//                 ^?
bar;
//^?

declare opaque type Baz;
//                   ^?

declare const baz: Baz;
//                 ^?
baz;
//^?

opaque type Bak<A> = number | A;
//           ^?

declare const bak: Bak<string>;
//                  ^?
bak;
//^?

declare opaque type Bam<A>
//                   ^?

declare const bam: Bam<string>;
//                  ^?
bam;
//^?

import type { Opaque, PolyTransparent, PolyOpaque } from './opaque-lib';

declare const opaque: Opaque;
//                     ^?
opaque;
// ^?

declare const polyTransparent: PolyTransparent<string>;
//                                 ^?
polyTransparent;
// ^?

declare const polyOpaque: PolyOpaque<string>;
//                          ^?
polyOpaque;
// ^?

import { fOpaque } from './opaque-lib';
const o = fOpaque();
//    ^?

opaque type OpaqueKeyWithSupertype1: string = string;
declare const opaqueDict: {[OpaqueKeyWithSupertype1]: number};
const x = Object.keys(opaqueDict)[0];
//    ^?

// ExactReactElement_DEPRECATED
import * as React from "react";
type Props = { prop: string, ... };
class MonoComponent extends React.Component<Props> {}
const monoElement = <MonoComponent prop={""}/>;
//    ^?
type PolyProps<T> = { prop: T, ... };
class PolyComponent<T> extends React.Component<PolyProps<T>> {}
const polyElement = <PolyComponent prop={""}/>;
//    ^?

const conditionalJsx = true ? <div /> : <span />;
//    ^?
