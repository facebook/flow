// @flow

import React from 'react';

React.startTransition(() => {}); // OK

function onClick() {};
React.startTransition(onClick) // OK

React.startTransition(); // Error: function requires another argument
