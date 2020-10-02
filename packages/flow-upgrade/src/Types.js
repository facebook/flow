/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

export type Upgrade = CodemodUpgrade;

export type CodemodUpgrade = {|
  +kind: 'codemod',
  +title: string,
  +description: string,
  +transformPath: string,
|};
