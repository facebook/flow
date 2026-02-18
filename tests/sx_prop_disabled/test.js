// @flow
import React from 'react';
import stylex from './stylex';

const styles = stylex.create({
  foo: { color: 'red' },
});

// With the flag off, sx is treated as a regular prop — not desugared.
<div sx={[styles.foo]} />; // ERROR — sx is not a known prop on div
