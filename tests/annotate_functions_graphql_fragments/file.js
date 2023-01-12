// @flow

import type {Foo as LocalFoo} from './export.graphql';
import {foo, bar} from './export.graphql';

/* should annotate x with `$NonMaybeType<LocalFoo?.["f"]?.["g"]?.["h"]?.[0]?.["i"]>` */
function m(x) {}

const x = foo()?.f?.g?.h?.[0]?.i;
if (x) {
  m(x);
}

const Bar = 1;

/* should annotate x with `$IMPORTED_TYPE$_Bar["f"]["g"]["h"][0]["i"]` */
function n(x) {}

const y = bar().f.g.h[0].i;
n(y);
