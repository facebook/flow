//@flow
import * as React from 'react';
component Foo() { return null }
component Bar() renders Foo { return <Foo /> }
component Bad() { return null }

const el1: renders Foo = <><Bar/></>; // OK
const el2: renders Bar = <><Bar/></>; // OK
const el3: renders Bad = <><Bar/></>; // ERROR
