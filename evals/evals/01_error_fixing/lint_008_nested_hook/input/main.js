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

export default component Counter() {
  hook useTick(): number {
    const [n] = useState(0);
    return n;
  }
  const t = useTick();
  return <span>{t}</span>;
}
