import * as React from 'react';
import {useRef} from 'react';

declare const foo: (mixed) => { current: void };

component Component() {
  const ref = useRef(null);
  const x = foo(ref); // Error
  return x.current;
}
