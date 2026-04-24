/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @noformat
 */

'use strict';

import type {ESNode} from 'flow-estree-oxidized';
import type {VisitorKeys as VisitorKeysType} from '../generated/ESTreeVisitorKeys';

import FlowVisitorKeys from '../generated/ESTreeVisitorKeys';

export function isNode(thing: mixed) /*: implies thing is {+[string]: mixed} */ {
  return (
    typeof thing === 'object' && thing != null && typeof thing.type === 'string'
  );
}

export type {VisitorKeysType};
export function getVisitorKeys<T: ESNode>(
  node: T,
  visitorKeys?: ?VisitorKeysType,
): $ReadOnlyArray<$Keys<T>> {
  const keys = (visitorKeys ?? FlowVisitorKeys)[node.type];
  if (keys == null) {
    throw new Error(`No visitor keys found for node type "${node.type}".`);
  }

  // $FlowExpectedError[prop-missing]
  return keys;
}
