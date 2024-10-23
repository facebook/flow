import * as React from 'react';
component Foo() { return null }
const FooBarBaz: React.ComponentType<{}> = React.memo(Foo);
export default FooBarBaz;
