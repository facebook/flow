// @flow

import type {Foo as LocalFoo} from './export.graphql';
import {foo, bar} from './export.graphql';

/* TODO should annotate x with `LocalFoo?.["f"]?.["g"]?.["h"]?.[0]?.["i"]` or equivalent type.
 * Currently the annotated type is missing the `?`. This seems to be a problem with the
 * inferred/normalized type -- not the graphql transform. */
export const x = foo()?.f?.g?.h?.[0]?.i;

/* should annotate x with `$IMPORTED_TYPE$_Bar["f"]["g"]["h"][0]["i"]` */
export const y = bar().f.g.h[0].i;
