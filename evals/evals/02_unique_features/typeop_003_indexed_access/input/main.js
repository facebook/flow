/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type ApiResponse = {
  user: {
    id: number,
    profile: {displayName: string, avatarUrl: string},
  },
  timestamp: number,
};

// TODO: Implement
