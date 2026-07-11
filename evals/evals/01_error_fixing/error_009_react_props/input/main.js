/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

import * as React from 'react';

component Button(
  label: string,
  onClick: () => void,
  disabled: boolean = false,
  size: 'small' | 'medium' | 'large' = 'medium',
) {
  return (
    <button onClick={onClick} disabled={disabled}>
      {label}
    </button>
  );
}

component App() {
  const handleClick = (): void => {};

  return (
    <div>
      {/* Flow error: missing required prop `onClick` */}
      <Button label="Click me" />

      {/* Flow error: extra prop `color` not in Button's props */}
      <Button label="Submit" onClick={handleClick} color="blue" />

      {/* Flow error: wrong type for `size` - "xl" is not in union */}
      <Button label="Big" onClick={handleClick} size="xl" />

      {/* This one should be fine */}
      <Button label="OK" onClick={handleClick} disabled={true} size="large" />
    </div>
  );
}
