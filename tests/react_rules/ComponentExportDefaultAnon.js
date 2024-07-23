import {useState} from 'react';

// This is a component because the basename is component-like
export default ({a}: {a: string}): React.Node => {
  useState(); // OK
  return a;
}
