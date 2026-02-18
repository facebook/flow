// @flow
import React from 'react';
import stylex from './stylex';

const styles = stylex.create({
  foo: { color: 'red' },
  bar: { backgroundColor: 'blue' },
});

<div sx={[styles.foo, styles.bar]} />; // OK
<div sx={[styles.foo]} />; // OK
<span sx={[styles.foo, styles.bar]} />; // OK

<div sx={[styles.foo, false, null]} />; // OK — boolean/nullish values (conditional styles)

<div sx={42} />; // ERROR — not an array
<div sx={[42]} />; // ERROR
<div sx={["not-a-style"]} />; // ERROR
