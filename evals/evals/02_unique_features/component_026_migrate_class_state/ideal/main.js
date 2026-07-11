/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';
import {useState} from 'react';

export default component Counter(step: number) {
  const [count, setCount] = useState(0);
  const increment = () => {
    setCount(prev => prev + step);
  };
  return (
    <div>
      <span>{count}</span>
      <button onClick={increment}>Add {step}</button>
    </div>
  );
}
