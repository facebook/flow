import React from 'react';

const undefinedValue = React.useDebugValue(123);

undefinedValue as typeof undefined; // Ok
undefinedValue as string; // Error: undefined is incompatible with string
