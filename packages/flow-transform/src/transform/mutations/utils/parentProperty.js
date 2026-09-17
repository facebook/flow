/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {ESNode} from 'flow-estree';

export function getParentProperty<T>(parent: ESNode, key: string): T {
  // $FlowExpectedError[prop-missing] Mutation keys are validated before use.
  return parent[key];
}

export function setParentProperty<T>(
  parent: ESNode,
  key: string,
  value: T,
): void {
  // $FlowExpectedError[prop-missing] Mutation keys are validated before use.
  parent[key] = value;
}
