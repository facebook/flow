/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as Formatters from 'Formatters';

const f = Formatters;

export function shout(name: string): string {
  return f.upper(name) + '!';
}
