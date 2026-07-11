/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

export default component Button(
  label: string,
  variant: 'primary' | 'secondary' = 'primary',
  disabled: boolean = false,
) {
  const className = `btn btn-${variant}`;
  return (
    <button className={className} disabled={disabled}>
      {label}
    </button>
  );
}
