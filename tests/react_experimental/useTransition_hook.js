import React from 'react';

React.useTransition(); // Ok

React.useTransition({}); // Error: no arguments are expected by function type

const [isPending, startTransition] = React.useTransition(); // OK

isPending as boolean; // Ok
startTransition as (() => void) => void; // Ok

isPending as (() => void) => void; // Error: boolean is incompatible with function type
startTransition as boolean; // Error: function type is incompatible with boolean

startTransition(() => {}); // Ok
startTransition(); // Error: function requires another argument
