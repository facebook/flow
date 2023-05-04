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
