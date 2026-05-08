/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

declare module 'mkdirp' {
  declare module.exports: {
    (dir: string, cb?: (err: Error, made: ?string) => void): Promise<?string>,
    sync(dir: string): ?string,
  };
}
