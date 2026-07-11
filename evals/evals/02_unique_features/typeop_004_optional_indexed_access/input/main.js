/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Config = ?{
  database?: {
    replica?: {
      host: string,
      port: number,
    },
  },
};

// TODO: Implement
