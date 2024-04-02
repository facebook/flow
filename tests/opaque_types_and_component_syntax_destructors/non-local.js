import {Context} from './opaque';
import {useContext} from 'react';
import * as React from 'react';

component Foo() {
  const context = useContext(Context);
  context as number; // ERROR
  return null;
}
