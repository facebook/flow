// @flow
import React from 'react';

// Without importing stylex, sx is treated as a regular prop even on lowercase elements.
<div sx={[1, 2, 3]} />; // ERROR — sx is not a known prop on div
