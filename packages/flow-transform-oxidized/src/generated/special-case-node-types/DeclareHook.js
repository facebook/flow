/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {
  DeclareHook as DeclareHookType,
  ESNode,
  FunctionTypeAnnotation as FunctionTypeAnnotationType,
} from 'hermes-estree';
import type {DetachedNode, MaybeDetachedNode} from '../../detachedNode';

import {
  asDetachedNode,
  detachedProps,
  setParentPointersInDirectChildren,
} from '../../detachedNode';

// the type annotation is stored on the Identifier's typeAnnotation
// which is super awkward to work with and type - so we flatten the input
// and put it in the right spot after
export type DeclareHookProps = {
  +name: string,
  +functionType: MaybeDetachedNode<FunctionTypeAnnotationType>,
};
export function DeclareHook(props: {
  ...$ReadOnly<DeclareHookProps>,
  +parent?: ESNode,
}): DetachedNode<DeclareHookType> {
  const node = detachedProps<DeclareHookType>(props.parent, {
    type: 'DeclareHook',
    id: detachedProps(null, {
      type: 'Identifier',
      name: props.name,
      typeAnnotation: detachedProps(null, {
        type: 'TypeAnnotation',
        typeAnnotation: asDetachedNode(props.functionType),
      }),
    }),
  });
  setParentPointersInDirectChildren(node);
  return node;
}
