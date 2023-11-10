import * as React from 'react';
import {useRef} from 'react';

component Component(value: number) {
  const ref = useRef<?number>(null);
  ref.current = value; // Error
  return ref.current; // Error
}
