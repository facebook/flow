import * as React from 'react';
component Foo() { return null }
// React$AbstractComponent vs. React.AbstractComponent in FooBarBaz5
const FooBarBaz: React$AbstractComponent<{}> = React.memo(Foo);
export default FooBarBaz;
