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
  ESNode,
  ObjectTypeAccessorSignature as ObjectTypeAccessorSignatureType,
  ObjectTypeMethodSignature as ObjectTypeMethodSignatureType,
  ObjectTypePropertySignature as ObjectTypePropertySignatureType,
} from 'flow-estree';
import type {DetachedNode, MaybeDetachedNode} from '../../detachedNode';

import {
  asDetachedNode,
  detachedProps,
  setParentPointersInDirectChildren,
} from '../../detachedNode';

export type ObjectTypeMethodSignatureProps = {
  readonly key: MaybeDetachedNode<ObjectTypeMethodSignatureType['key']>,
  readonly value: MaybeDetachedNode<ObjectTypeMethodSignatureType['value']>,

  // optional because they only apply to the class decl case
  readonly static?: ObjectTypeMethodSignatureType['static'],
  readonly proto?: ObjectTypeMethodSignatureType['proto'],
};
export function ObjectTypeMethodSignature(props: {
  ...Readonly<ObjectTypeMethodSignatureProps>,
  readonly parent?: ESNode,
}): DetachedNode<ObjectTypeMethodSignatureType> {
  const node = detachedProps<ObjectTypeMethodSignatureType>(props.parent, {
    type: 'ObjectTypeProperty',
    key: asDetachedNode(props.key),
    kind: 'init',
    method: true,
    optional: false,
    proto: props.proto ?? false,
    static: props.static ?? false,
    value: asDetachedNode(props.value),
    variance: null,
  });
  setParentPointersInDirectChildren(node);
  return node;
}

export type ObjectTypePropertySignatureProps = {
  readonly key: MaybeDetachedNode<ObjectTypePropertySignatureType['key']>,
  readonly value: MaybeDetachedNode<ObjectTypePropertySignatureType['value']>,
  readonly optional: ObjectTypePropertySignatureType['optional'],
  readonly variance?: ?MaybeDetachedNode<
    ObjectTypePropertySignatureType['variance'],
  >,

  // optional because they only apply to the class decl case
  readonly static?: ObjectTypeMethodSignatureType['static'],
  readonly proto?: ObjectTypeMethodSignatureType['proto'],
};
export function ObjectTypePropertySignature(props: {
  ...Readonly<ObjectTypePropertySignatureProps>,
  readonly parent?: ESNode,
}): DetachedNode<ObjectTypePropertySignatureType> {
  const node = detachedProps<ObjectTypePropertySignatureType>(props.parent, {
    type: 'ObjectTypeProperty',
    key: asDetachedNode(props.key),
    kind: 'init',
    method: false,
    optional: props.optional,
    proto: props.proto ?? false,
    static: props.static ?? false,
    value: asDetachedNode(props.value),
    // $FlowFixMe[incompatible-type]
    variance: asDetachedNode(props.variance),
  });
  setParentPointersInDirectChildren(node);
  return node;
}

export type ObjectTypeAccessorSignatureProps = {
  readonly key: MaybeDetachedNode<ObjectTypeAccessorSignatureType['key']>,
  readonly value: MaybeDetachedNode<ObjectTypeAccessorSignatureType['value']>,
  readonly kind: ObjectTypeAccessorSignatureType['kind'],

  // optional because they only apply to the class decl case
  readonly static?: ObjectTypeMethodSignatureType['static'],
  readonly proto?: ObjectTypeMethodSignatureType['proto'],
};
export function ObjectTypeAccessorSignature(props: {
  ...Readonly<ObjectTypeAccessorSignatureProps>,
  readonly parent?: ESNode,
}): DetachedNode<ObjectTypeAccessorSignatureType> {
  const node = detachedProps<ObjectTypeAccessorSignatureType>(props.parent, {
    type: 'ObjectTypeProperty',
    key: asDetachedNode(props.key),
    kind: props.kind,
    method: false,
    optional: false,
    proto: props.proto ?? false,
    static: props.static ?? false,
    value: asDetachedNode(props.value),
    variance: null,
  });
  setParentPointersInDirectChildren(node);
  return node;
}
