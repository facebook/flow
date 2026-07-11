/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Event =
  | {kind: 'click', button: 'left' | 'right'}
  | {kind: 'key', button: 'up' | 'down'};

// TODO: Implement
