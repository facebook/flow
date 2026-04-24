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
  DestructuringObjectProperty as DestructuringObjectPropertyType,
  DestructuringObjectPropertyWithComputedName as DestructuringObjectPropertyWithComputedNameType,
  DestructuringObjectPropertyWithNonShorthandStaticName as DestructuringObjectPropertyWithNonShorthandStaticNameType,
  DestructuringObjectPropertyWithShorthandStaticName as DestructuringObjectPropertyWithShorthandStaticNameType,
  ESNode,
  ObjectProperty as ObjectPropertyType,
  ObjectPropertyWithComputedName as ObjectPropertyWithComputedNameType,
  ObjectPropertyWithNonShorthandStaticName as ObjectPropertyWithNonShorthandStaticNameType,
  ObjectPropertyWithShorthandStaticName as ObjectPropertyWithShorthandStaticNameType,
} from 'hermes-estree';
import type {DetachedNode, MaybeDetachedNode} from '../../detachedNode';

import {
  asDetachedNode,
  detachedProps,
  setParentPointersInDirectChildren,
} from '../../detachedNode';

export type DestructuringObjectPropertyProps = {
  +key: MaybeDetachedNode<DestructuringObjectPropertyType['key']>,
  +value: MaybeDetachedNode<DestructuringObjectPropertyType['value']>,
  +computed: DestructuringObjectPropertyType['computed'],
  +shorthand: DestructuringObjectPropertyType['shorthand'],
};
export function DestructuringObjectProperty(props: {
  ...$ReadOnly<DestructuringObjectPropertyProps>,
  +parent?: ESNode,
}): DetachedNode<DestructuringObjectPropertyType> {
  const node = detachedProps<DestructuringObjectPropertyType>(props.parent, {
    type: 'Property',
    kind: 'init',
    method: false,
    key: asDetachedNode(props.key),
    value: asDetachedNode(props.value),
    computed: props.computed,
    shorthand: props.shorthand,
  });
  setParentPointersInDirectChildren(node);
  return node;
}

export type DestructuringObjectPropertyWithNonShorthandStaticNameProps = {
  +key: MaybeDetachedNode<
    DestructuringObjectPropertyWithNonShorthandStaticNameType['key'],
  >,
  +value: MaybeDetachedNode<
    DestructuringObjectPropertyWithNonShorthandStaticNameType['value'],
  >,
};
export function DestructuringObjectPropertyWithNonShorthandStaticName(props: {
  ...$ReadOnly<DestructuringObjectPropertyWithNonShorthandStaticNameProps>,
  +parent?: ESNode,
}): DetachedNode<DestructuringObjectPropertyWithNonShorthandStaticNameType> {
  const node =
    detachedProps<DestructuringObjectPropertyWithNonShorthandStaticNameType>(
      props.parent,
      {
        type: 'Property',
        kind: 'init',
        method: false,
        key: asDetachedNode(props.key),
        value: asDetachedNode(props.value),
        computed: false,
        shorthand: false,
      },
    );
  setParentPointersInDirectChildren(node);
  return node;
}

export type DestructuringObjectPropertyWithShorthandStaticNameProps = {
  +key: MaybeDetachedNode<
    DestructuringObjectPropertyWithShorthandStaticNameType['key'],
  >,
  +value: MaybeDetachedNode<
    DestructuringObjectPropertyWithShorthandStaticNameType['value'],
  >,
};
export function DestructuringObjectPropertyWithShorthandStaticName(props: {
  ...$ReadOnly<DestructuringObjectPropertyWithShorthandStaticNameProps>,
  +parent?: ESNode,
}): DetachedNode<DestructuringObjectPropertyWithShorthandStaticNameType> {
  const node =
    detachedProps<DestructuringObjectPropertyWithShorthandStaticNameType>(
      props.parent,
      {
        type: 'Property',
        kind: 'init',
        method: false,
        key: asDetachedNode(props.key),
        value: asDetachedNode(props.value),
        computed: false,
        shorthand: true,
      },
    );
  setParentPointersInDirectChildren(node);
  return node;
}

export type DestructuringObjectPropertyWithComputedNameProps = {
  +key: MaybeDetachedNode<
    DestructuringObjectPropertyWithComputedNameType['key'],
  >,
  +value: MaybeDetachedNode<
    DestructuringObjectPropertyWithComputedNameType['value'],
  >,
};
export function DestructuringObjectPropertyWithComputedName(props: {
  ...$ReadOnly<DestructuringObjectPropertyWithComputedNameProps>,
  +parent?: ESNode,
}): DetachedNode<DestructuringObjectPropertyWithComputedNameType> {
  const node = detachedProps<DestructuringObjectPropertyWithComputedNameType>(
    props.parent,
    {
      type: 'Property',
      kind: 'init',
      method: false,
      key: asDetachedNode(props.key),
      value: asDetachedNode(props.value),
      computed: true,
      shorthand: false,
    },
  );
  setParentPointersInDirectChildren(node);
  return node;
}

export type ObjectPropertyProps = {
  +key: MaybeDetachedNode<ObjectPropertyType['key']>,
  +value: MaybeDetachedNode<ObjectPropertyType['value']>,
  +kind: ObjectPropertyType['kind'],
  +computed: ObjectPropertyType['computed'],
  +method: ObjectPropertyType['method'],
  +shorthand: ObjectPropertyType['shorthand'],
};
export function ObjectProperty(props: {
  ...$ReadOnly<ObjectPropertyProps>,
  +parent?: ESNode,
}): DetachedNode<ObjectPropertyType> {
  const node = detachedProps<ObjectPropertyType>(props.parent, {
    type: 'Property',
    key: asDetachedNode(props.key),
    kind: props.kind,
    value: asDetachedNode(props.value),
    computed: props.computed,
    method: props.method,
    shorthand: props.shorthand,
  });
  setParentPointersInDirectChildren(node);
  return node;
}

export type ObjectPropertyWithNonShorthandStaticNameProps = {
  +key: MaybeDetachedNode<ObjectPropertyWithNonShorthandStaticNameType['key']>,
  +value: MaybeDetachedNode<
    ObjectPropertyWithNonShorthandStaticNameType['value'],
  >,
  +kind: ObjectPropertyWithNonShorthandStaticNameType['kind'],
  +method: ObjectPropertyWithNonShorthandStaticNameType['method'],
};
export function ObjectPropertyWithNonShorthandStaticName(props: {
  ...$ReadOnly<ObjectPropertyWithNonShorthandStaticNameProps>,
  +parent?: ESNode,
}): DetachedNode<ObjectPropertyWithNonShorthandStaticNameType> {
  const node = detachedProps<ObjectPropertyWithNonShorthandStaticNameType>(
    props.parent,
    {
      type: 'Property',
      key: asDetachedNode(props.key),
      kind: props.kind,
      value: asDetachedNode(props.value),
      computed: false,
      method: props.method,
      shorthand: false,
    },
  );
  setParentPointersInDirectChildren(node);
  return node;
}

export type ObjectPropertyWithShorthandStaticNameProps = {
  +key: MaybeDetachedNode<ObjectPropertyWithShorthandStaticNameType['key']>,
  +value: MaybeDetachedNode<ObjectPropertyWithShorthandStaticNameType['value']>,
};
export function ObjectPropertyWithShorthandStaticName(props: {
  ...$ReadOnly<ObjectPropertyWithShorthandStaticNameProps>,
  +parent?: ESNode,
}): DetachedNode<ObjectPropertyWithShorthandStaticNameType> {
  const node = detachedProps<ObjectPropertyWithShorthandStaticNameType>(
    props.parent,
    {
      type: 'Property',
      key: asDetachedNode(props.key),
      kind: 'init',
      value: asDetachedNode(props.value),
      computed: false,
      method: false,
      shorthand: true,
    },
  );
  setParentPointersInDirectChildren(node);
  return node;
}

export type ObjectPropertyWithComputedNameProps = {
  +key: MaybeDetachedNode<ObjectPropertyWithComputedNameType['key']>,
  +value: MaybeDetachedNode<ObjectPropertyWithComputedNameType['value']>,
  +kind: ObjectPropertyWithComputedNameType['kind'],
  +method: ObjectPropertyWithComputedNameType['method'],
};
export function ObjectPropertyWithComputedName(props: {
  ...$ReadOnly<ObjectPropertyWithComputedNameProps>,
  +parent?: ESNode,
}): DetachedNode<ObjectPropertyWithComputedNameType> {
  const node = detachedProps<ObjectPropertyWithComputedNameType>(props.parent, {
    type: 'Property',
    key: asDetachedNode(props.key),
    kind: props.kind,
    value: asDetachedNode(props.value),
    computed: true,
    method: props.method,
    shorthand: false,
  });
  setParentPointersInDirectChildren(node);
  return node;
}
