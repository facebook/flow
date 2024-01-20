// @flow

export opaque type Foo = number;
//                 ^

declare var foo: Foo;
//                ^
foo;
//^

declare opaque type Bar: number;
//                   ^

declare var bar: Bar;
//               ^
bar;
//^

declare opaque type Baz;
//                   ^

declare var baz: Baz;
//               ^
baz;
//^

opaque type Bak<A> = number | A;
//           ^

declare var bak: Bak<string>;
//                ^
bak;
//^

declare opaque type Bam<A>
//                   ^

declare var bam: Bam<string>;
//                ^
bam;
//^

import type { Opaque, PolyTransparent, PolyOpaque } from './opaque-lib';

declare var opaque: Opaque;
//                   ^
opaque;
// ^

declare var polyTransparent: PolyTransparent<string>;
//                               ^
polyTransparent;
// ^

declare var polyOpaque: PolyOpaque<string>;
//                        ^
polyOpaque;
// ^

import { fOpaque } from './opaque-lib';
const o = fOpaque();
//    ^

opaque type OpaqueKeyWithSupertype1: string = string;
declare const opaqueDict: {[OpaqueKeyWithSupertype1]: number};
const x = Object.keys(opaqueDict)[0];
//    ^

// React$Element
import * as React from "react";
type Props = { prop: string, ... };
class MonoComponent extends React.Component<Props> {}
const monoElement = <MonoComponent prop={""}/>;
//    ^
type PolyProps<T> = { prop: T, ... };
class PolyComponent<T> extends React.Component<PolyProps<T>> {}
const polyElement = <PolyComponent prop={""}/>;
//    ^
