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
  DeclareExportDefaultDeclaration as DeclareExportDefaultDeclarationType,
  ESNode,
  DeclareExportDeclarationNamedWithDeclaration as DeclareExportDeclarationNamedWithDeclarationType,
  DeclareExportDeclarationNamedWithSpecifiers as DeclareExportDeclarationNamedWithSpecifiersType,
} from 'flow-estree';
import type {DetachedNode, MaybeDetachedNode} from '../../detachedNode';

import {
  asDetachedNode,
  detachedProps,
  setParentPointersInDirectChildren,
} from '../../detachedNode';

export type DeclareExportDefaultDeclarationProps = {
  readonly declaration?: ?MaybeDetachedNode<
    DeclareExportDefaultDeclarationType['declaration'],
  >,
};
export function DeclareExportDefaultDeclaration(props: {
  ...Readonly<DeclareExportDefaultDeclarationProps>,
  readonly parent?: ESNode,
}): DetachedNode<DeclareExportDefaultDeclarationType> {
  const node = detachedProps<DeclareExportDefaultDeclarationType>(
    props.parent,
    {
      type: 'DeclareExportDeclaration',
      // $FlowFixMe[incompatible-type]
      declaration: asDetachedNode(props.declaration),
      specifiers: [],
      source: null,
      default: true,
    },
  );
  setParentPointersInDirectChildren(node);
  return node;
}

export type DeclareExportDeclarationNamedWithDeclarationProps = {
  readonly declaration?: ?MaybeDetachedNode<
    DeclareExportDeclarationNamedWithDeclarationType['declaration'],
  >,
};
export function DeclareExportDeclarationNamedWithDeclaration(props: {
  ...Readonly<DeclareExportDeclarationNamedWithDeclarationProps>,
  readonly parent?: ESNode,
}): DetachedNode<DeclareExportDeclarationNamedWithDeclarationType> {
  const node = detachedProps<DeclareExportDeclarationNamedWithDeclarationType>(
    props.parent,
    {
      type: 'DeclareExportDeclaration',
      // $FlowFixMe[incompatible-type]
      declaration: asDetachedNode(props.declaration),
      specifiers: [],
      source: null,
      default: false,
    },
  );
  setParentPointersInDirectChildren(node);
  return node;
}

export type DeclareExportDeclarationNamedWithSpecifiersProps = {
  readonly specifiers: ReadonlyArray<
    MaybeDetachedNode<
      DeclareExportDeclarationNamedWithSpecifiersType['specifiers'][number],
    >,
  >,
  readonly source?: ?MaybeDetachedNode<
    DeclareExportDeclarationNamedWithSpecifiersType['source'],
  >,
};
export function DeclareExportDeclarationNamedWithSpecifiers(props: {
  ...Readonly<DeclareExportDeclarationNamedWithSpecifiersProps>,
  readonly parent?: ESNode,
}): DetachedNode<DeclareExportDeclarationNamedWithSpecifiersType> {
  const node = detachedProps<DeclareExportDeclarationNamedWithSpecifiersType>(
    props.parent,
    {
      type: 'DeclareExportDeclaration',
      declaration: null,
      specifiers: props.specifiers.map(n => asDetachedNode(n)),
      // $FlowFixMe[incompatible-type]
      source: asDetachedNode(props.source),
      default: false,
    },
  );
  setParentPointersInDirectChildren(node);
  return node;
}
