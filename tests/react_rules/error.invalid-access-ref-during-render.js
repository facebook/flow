import * as React from 'react';
import {useRef} from 'react';

declare const foo: any;

component Component() {
  const ref = useRef(null);
  const value = ref.current; // Error
  return value;
}
