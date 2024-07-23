import {useState} from 'react';

// This is not a component because the basename is not component-like
export default ({a}: {a: string}): React.Node => {
  useState(); // ERROR
  return a;
}
