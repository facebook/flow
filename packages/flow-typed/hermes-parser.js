/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

declare module 'hermes-parser' {
  declare export function parse(code: string, opts?: mixed): mixed;
  declare export function parseForESLint(code: string, opts?: mixed): mixed;
  declare export var FlowVisitorKeys: mixed;
  declare export var SimpleTransform: mixed;
  declare export var SimpleTraverser: mixed;
}
