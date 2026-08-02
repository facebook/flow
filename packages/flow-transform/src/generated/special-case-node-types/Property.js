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
} from 'flow-estree';
import type {DetachedNode, MaybeDetachedNode} from '../../detachedNode';

import {
  asDetachedNode,
  detachedProps,
  setParentPointersInDirectChildren,
} from '../../detachedNode';

export type DestructuringObjectPropertyProps = {
  readonly key: MaybeDetachedNode<DestructuringObjectPropertyType['key']>,
  readonly value: MaybeDetachedNode<DestructuringObjectPropertyType['value']>,
  readonly computed: DestructuringObjectPropertyType['computed'],
  readonly shorthand: DestructuringObjectPropertyType['shorthand'],
};
export function DestructuringObjectProperty(props: {
  ...Readonly<DestructuringObjectPropertyProps>,
  readonly parent?: ESNode,
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
  readonly key: MaybeDetachedNode<
    DestructuringObjectPropertyWithNonShorthandStaticNameType['key'],
  >,
  readonly value: MaybeDetachedNode<
    DestructuringObjectPropertyWithNonShorthandStaticNameType['value'],
  >,
};
export function DestructuringObjectPropertyWithNonShorthandStaticName(props: {
  ...Readonly<DestructuringObjectPropertyWithNonShorthandStaticNameProps>,
  readonly parent?: ESNode,
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
  readonly key: MaybeDetachedNode<
    DestructuringObjectPropertyWithShorthandStaticNameType['key'],
  >,
  readonly value: MaybeDetachedNode<
    DestructuringObjectPropertyWithShorthandStaticNameType['value'],
  >,
};
export function DestructuringObjectPropertyWithShorthandStaticName(props: {
  ...Readonly<DestructuringObjectPropertyWithShorthandStaticNameProps>,
  readonly parent?: ESNode,
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
  readonly key: MaybeDetachedNode<
    DestructuringObjectPropertyWithComputedNameType['key'],
  >,
  readonly value: MaybeDetachedNode<
    DestructuringObjectPropertyWithComputedNameType['value'],
  >,
};
export function DestructuringObjectPropertyWithComputedName(props: {
  ...Readonly<DestructuringObjectPropertyWithComputedNameProps>,
  readonly parent?: ESNode,
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
  readonly key: MaybeDetachedNode<ObjectPropertyType['key']>,
  readonly value: MaybeDetachedNode<ObjectPropertyType['value']>,
  readonly kind: ObjectPropertyType['kind'],
  readonly computed: ObjectPropertyType['computed'],
  readonly method: ObjectPropertyType['method'],
  readonly shorthand: ObjectPropertyType['shorthand'],
};
export function ObjectProperty(props: {
  ...Readonly<ObjectPropertyProps>,
  readonly parent?: ESNode,
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
  readonly key: MaybeDetachedNode<
    ObjectPropertyWithNonShorthandStaticNameType['key'],
  >,
  readonly value: MaybeDetachedNode<
    ObjectPropertyWithNonShorthandStaticNameType['value'],
  >,
  readonly kind: ObjectPropertyWithNonShorthandStaticNameType['kind'],
  readonly method: ObjectPropertyWithNonShorthandStaticNameType['method'],
};
export function ObjectPropertyWithNonShorthandStaticName(props: {
  ...Readonly<ObjectPropertyWithNonShorthandStaticNameProps>,
  readonly parent?: ESNode,
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
  readonly key: MaybeDetachedNode<
    ObjectPropertyWithShorthandStaticNameType['key'],
  >,
  readonly value: MaybeDetachedNode<
    ObjectPropertyWithShorthandStaticNameType['value'],
  >,
};
export function ObjectPropertyWithShorthandStaticName(props: {
  ...Readonly<ObjectPropertyWithShorthandStaticNameProps>,
  readonly parent?: ESNode,
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
  readonly key: MaybeDetachedNode<ObjectPropertyWithComputedNameType['key']>,
  readonly value: MaybeDetachedNode<
    ObjectPropertyWithComputedNameType['value'],
  >,
  readonly kind: ObjectPropertyWithComputedNameType['kind'],
  readonly method: ObjectPropertyWithComputedNameType['method'],
};
export function ObjectPropertyWithComputedName(props: {
  ...Readonly<ObjectPropertyWithComputedNameProps>,
  readonly parent?: ESNode,
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
