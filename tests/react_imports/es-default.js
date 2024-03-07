import React from 'react';

(React.Component: Object); // OK
(React.Component: number); // Error
('Hello, world!': React.Node); // Ok: in namespace.
({}: React.Node); // Error.
(null: React.Missing); // Error: Not in default export.
