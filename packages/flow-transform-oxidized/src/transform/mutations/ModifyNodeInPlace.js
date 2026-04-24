/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {ESNode} from 'flow-estree-oxidized';
import type {MutationContext} from '../MutationContext';

import {isNode} from 'flow-parser-oxidized';

export type ModifyNodeInPlaceMutation = $ReadOnly<{
  type: 'modifyNodeInPlace',
  target: ESNode,
  newProps: $ReadOnly<{...}>,
}>;

export function createModifyNodeInPlaceMutation(
  target: ModifyNodeInPlaceMutation['target'],
  newProps: ModifyNodeInPlaceMutation['newProps'],
): ModifyNodeInPlaceMutation {
  return {
    type: 'modifyNodeInPlace',
    target,
    newProps,
  };
}

export function performModifyNodeInPlaceMutation(
  mutationContext: MutationContext,
  mutation: ModifyNodeInPlaceMutation,
): ESNode {
  const {target, newProps} = mutation;

  for (const [key, newPropValue] of Object.entries(newProps)) {
    const prevPropValue = target[key];

    // If the value did not change, skip.
    // $FlowFixMe[invalid-compare]
    if (prevPropValue === newPropValue) {
      continue;
    }

    // Mark removed nodes as deleted
    if (isNode(prevPropValue)) {
      mutationContext.markDeletion(prevPropValue);
    } else if (Array.isArray(prevPropValue)) {
      for (const prevPropValueItem of prevPropValue) {
        if (isNode(prevPropValueItem)) {
          mutationContext.markDeletion(prevPropValueItem);
        }
      }
    }

    // Mark node property as mutated.
    mutationContext.markMutation(target, key);

    // Assign new property.
    target[key] = newPropValue;
  }

  return target;
}
