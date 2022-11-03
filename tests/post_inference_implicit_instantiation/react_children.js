// @flow

import * as React from 'react';

React.Children.toArray(1); // underconstrained
React.Children.toArray([1]); // underconstrained
