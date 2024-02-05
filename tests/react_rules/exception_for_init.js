import * as React from 'react';
import {useRef} from 'react';

component Component() {
  const ref1 = useRef<?number>(null);
  if (ref1.current === null) {
    ref1.current = 42; // ok
  }
  ref1.current; // error

  const ref2 = useRef<?number>(null);
  if (ref2.current === null) {
    ref2.current; // error
  }

  const ref3 = useRef<?number>(null);
  if (ref3.current === null && ref1) {
    ref3.current = 42; // ok
  }

  const ref4 = useRef<?number>(null);
  if (ref4.current === null || ref1) {
    ref4.current = 42; // error
  }

  const ref5 = useRef<?number>(null);
  if (ref5.current == undefined) {
    ref5.current = 42; // ok
  }

  const ref6 = useRef<?number>(null);
  if (!ref6.current) {
    ref6.current = 42; // ok
  }
  return null;
}
