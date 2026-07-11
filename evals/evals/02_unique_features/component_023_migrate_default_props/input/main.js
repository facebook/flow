/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

type Props = {
  label: string,
  variant?: 'primary' | 'secondary',
  disabled?: boolean,
};

function Button({label, variant = 'primary', disabled = false}: Props): React.Node {
  const className = `btn btn-${variant}`;
  return (
    <button className={className} disabled={disabled}>
      {label}
    </button>
  );
}

export default Button;
