import type * as React from 'react'; // React is namespace, not an object type

type A = React.Node; // has access to types
// limited access to values
type B = typeof React.Component; // ok
React.act; // error: 'React' cannot be used as a value because it was imported using 'import type'
