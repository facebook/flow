import React from 'react';

const deferredValue = React.useDeferredValue(true); // Ok
deferredValue as boolean; // Ok

React.useDeferredValue(true, {}); // Ok: React 19 added an optional initial-value second argument
