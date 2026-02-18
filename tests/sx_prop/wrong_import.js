// @flow
import React from 'react';
import notStylex from './stylex';

// Importing stylex under a different name means sx is not desugared.
<div sx={[1, 2, 3]} />; // ERROR — sx is not a known prop on div
