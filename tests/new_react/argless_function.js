//@flow

const React = require('react');

function Component(): React.Node { return null; }

const element = <Component />;

declare const x: React.ElementProps<typeof Component>;
x.foo = 3; // Error, the props type for Component is a sealed empty object.
