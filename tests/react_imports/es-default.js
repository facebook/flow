import React from 'react';

React.Component as Object; // OK
React.Component as number; // Error
'Hello, world!' as React.Node; // Ok: in namespace.
({} as React.Node); // Error.
null as React.Missing; // Error: Not in default export.
