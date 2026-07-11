/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

component PriceTag(label: string, amount: number) {
  return <span>{label}: {'$' + amount.toFixed(2)}</span>;
}
