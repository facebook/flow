/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

declare module 'lz-string' {
  declare export function decompressFromEncodedURIComponent(s: string): string;
  declare export function compressToEncodedURIComponent(s: string): string;
}
