/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import {fromDollars, add, scale, format} from './main';

const subtotal = add(fromDollars(19.99), fromDollars(5.5));
const withTax = scale(subtotal, 1.08);

export const receipt: string = format(withTax);
