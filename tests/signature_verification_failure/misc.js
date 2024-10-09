//@flow

import * as React from 'react';

export const c = {['a' + 'b']: 42};

export const d = [...c];

export const e = (d += d);

export const f = class {};

// export const u = 42n; TODO: type sig for bigints

export const w = /foo/;

export const x = <foo />;
