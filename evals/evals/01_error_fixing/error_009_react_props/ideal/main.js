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
      <Button label="Click me" onClick={handleClick} />

      <Button label="Submit" onClick={handleClick} />

      <Button label="Big" onClick={handleClick} size="large" />

      <Button label="OK" onClick={handleClick} disabled={true} size="large" />
    </div>
  );
}
