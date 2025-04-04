import * as React from 'react';

function test_intrinsics() {
  const MyDiv = 'div';
  const e1 = <MyDiv id={"a"}/>; // okay
  const e2 = <MyDiv
    id={1} // error on id number ~> string
    onClick={e => {
      e as empty; // error empty ~> Event
    }}
/>;
}
