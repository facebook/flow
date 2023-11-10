import * as React from 'react';
import {useRef} from 'react';

declare const Foo: any;

component Component() {
  const ref = useRef(null);
  return <Foo ref={ref} />; // No error
}
