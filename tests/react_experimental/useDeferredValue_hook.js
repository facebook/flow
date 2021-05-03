// @flow

import React from 'react';

const deferredValue = React.useDeferredValue(true); // Ok
(deferredValue: boolean); // Ok

React.useDeferredValue(true, {}); // Error: no more than 1 argument is expected by function type
