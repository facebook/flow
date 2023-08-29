//@flow

import * as React from 'react';

component Fragment<T: React.Node>(children: T) renders T {
  return children;
}

const el = <Fragment>{null}</Fragment>;
(el: $Renders<null>); // no error
(el: $Renders<void>); // error

component Foo() { return null }
component Bar() {return null }

component FooContainer(children: $Renders<Foo>) { return null };

const transitive = <FooContainer><Fragment><Foo /></Fragment></FooContainer>; // ok
const transitiveBad = <FooContainer><Fragment><Bar /></Fragment></FooContainer>; // error

component Baz() renders Foo { return <Foo /> }

const doubleTransitive = <FooContainer><Fragment><Baz /></Fragment></FooContainer>; // ok

component Menu(children: $Renders<React.ChildrenArray<$Renders<MenuItem>>>) { return null; }
component MenuItem() { return null; }

const menu = ( // OK!
  <Menu>
    <Fragment>
      <MenuItem />
      <MenuItem />
      <MenuItem />
    </Fragment>
 </Menu>
)
