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
  ExportNamedDeclarationWithDeclaration as ExportNamedDeclarationWithDeclarationType,
  ExportNamedDeclarationWithSpecifiers as ExportNamedDeclarationWithSpecifiersType,
} from 'flow-estree';
import type {DetachedNode, MaybeDetachedNode} from '../../detachedNode';

import {
  asDetachedNode,
  detachedProps,
  setParentPointersInDirectChildren,
} from '../../detachedNode';

export type ExportNamedDeclarationWithDeclarationProps = {
  readonly declaration?: ?MaybeDetachedNode<
    ExportNamedDeclarationWithDeclarationType['declaration'],
  >,
  readonly exportKind: ExportNamedDeclarationWithDeclarationType['exportKind'],
};
export function ExportNamedDeclarationWithDeclaration(props: {
  ...Readonly<ExportNamedDeclarationWithDeclarationProps>,
  readonly parent?: ESNode,
}): DetachedNode<ExportNamedDeclarationWithDeclarationType> {
  const node = detachedProps<ExportNamedDeclarationWithDeclarationType>(
    props.parent,
    {
      type: 'ExportNamedDeclaration',
      // $FlowFixMe[incompatible-type]
      declaration: asDetachedNode(props.declaration),
      specifiers: [],
      source: null,
      exportKind: props.exportKind,
    },
  );
  setParentPointersInDirectChildren(node);
  return node;
}

export type ExportNamedDeclarationWithSpecifiersProps = {
  readonly specifiers: ReadonlyArray<
    MaybeDetachedNode<
      ExportNamedDeclarationWithSpecifiersType['specifiers'][number],
    >,
  >,
  readonly source?: ?MaybeDetachedNode<
    ExportNamedDeclarationWithSpecifiersType['source'],
  >,
  readonly exportKind: ExportNamedDeclarationWithSpecifiersType['exportKind'],
};
export function ExportNamedDeclarationWithSpecifiers(props: {
  ...Readonly<ExportNamedDeclarationWithSpecifiersProps>,
  readonly parent?: ESNode,
}): DetachedNode<ExportNamedDeclarationWithSpecifiersType> {
  const node = detachedProps<ExportNamedDeclarationWithSpecifiersType>(
    props.parent,
    {
      type: 'ExportNamedDeclaration',
      declaration: null,
      specifiers: props.specifiers.map(n => asDetachedNode(n)),
      // $FlowFixMe[incompatible-type]
      source: asDetachedNode(props.source),
      exportKind: props.exportKind,
    },
  );
  setParentPointersInDirectChildren(node);
  return node;
}
