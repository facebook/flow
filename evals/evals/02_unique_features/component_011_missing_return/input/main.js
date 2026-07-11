/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

component CartSummary(itemCount: number, total: number) {
  if (itemCount === 0) {
    return <div>Your cart is empty</div>;
  }
  if (itemCount > 0) {
    return <div>{itemCount} items · {'$' + total.toFixed(2)}</div>;
  }
}
