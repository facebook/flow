/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

export default component AccessibleButton(
  'aria-label' as label: string,
  'aria-description' as description?: string = "",
  'data-role' as role?: string = "button",
  onClick: () => void,
  children: React.Node,
) {
  return (
    <button
      aria-label={label}
      aria-description={description}
      data-testid={"btn-" + role}
      onClick={onClick}
    >
      {children}
    </button>
  );
}
