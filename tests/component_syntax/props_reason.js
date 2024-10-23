import * as React from 'react';

component Foo(x: number) { return null; }

// This error should mention the props of Foo
const a: React.ComponentType<mixed> = Foo; // ERROR
