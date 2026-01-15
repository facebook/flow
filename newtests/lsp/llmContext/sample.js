// @flow

import * as React from 'react';
import {something} from './utils';
import type {MyType} from './types';

type Props = {
  name: string,
  age: number,
  isActive?: boolean,
};

export default component UserProfile(
  name: string,
  age: number,
  isActive?: boolean = false,
) {
  return (
    <div>
      <h1>{name}</h1>
      <p>Age: {age}</p>
      {isActive && <span>Active</span>}
    </div>
  );
}
