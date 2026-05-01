import * as React from 'react';

React.Component as Object; // OK
React.Component as number; // Error
'Hello, world!' as React.Node; // OK
({} as React.Node); // Error
null as React.Missing; // Error
