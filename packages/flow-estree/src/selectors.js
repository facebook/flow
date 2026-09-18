/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

'use strict';

import type {
  AbstractMethodDefinition,
  AbstractPropertyDefinition,
  ConstructorTypeAnnotation,
  DeclareClassExtendsCall,
  ExportAssignment,
  ExternalModuleReference,
  ImportEqualsDeclaration,
  ImportType,
  NamespaceExportDeclaration,
  ObjectTypePrivateField,
  SatisfiesExpression,
  TemplateLiteralTypeAnnotation,
  TupleTypeElement,
} from './types';
import type {ESQueryNodeSelectorsWithoutFallback as HermesESQueryNodeSelectorsWithoutFallback} from './generated/HermesESTreeSelectorTypes';

export type ESQueryNodeSelectorsWithoutFallback = Readonly<{
  ...HermesESQueryNodeSelectorsWithoutFallback,
  readonly AbstractMethodDefinition?: (node: AbstractMethodDefinition) => void,
  readonly 'AbstractMethodDefinition:exit'?: (
    node: AbstractMethodDefinition,
  ) => void,
  readonly AbstractPropertyDefinition?: (
    node: AbstractPropertyDefinition,
  ) => void,
  readonly 'AbstractPropertyDefinition:exit'?: (
    node: AbstractPropertyDefinition,
  ) => void,
  readonly ConstructorTypeAnnotation?: (
    node: ConstructorTypeAnnotation,
  ) => void,
  readonly 'ConstructorTypeAnnotation:exit'?: (
    node: ConstructorTypeAnnotation,
  ) => void,
  readonly DeclareClassExtendsCall?: (node: DeclareClassExtendsCall) => void,
  readonly 'DeclareClassExtendsCall:exit'?: (
    node: DeclareClassExtendsCall,
  ) => void,
  readonly ExportAssignment?: (node: ExportAssignment) => void,
  readonly 'ExportAssignment:exit'?: (node: ExportAssignment) => void,
  readonly ExternalModuleReference?: (node: ExternalModuleReference) => void,
  readonly 'ExternalModuleReference:exit'?: (
    node: ExternalModuleReference,
  ) => void,
  readonly ImportEqualsDeclaration?: (node: ImportEqualsDeclaration) => void,
  readonly 'ImportEqualsDeclaration:exit'?: (
    node: ImportEqualsDeclaration,
  ) => void,
  readonly ImportType?: (node: ImportType) => void,
  readonly 'ImportType:exit'?: (node: ImportType) => void,
  readonly NamespaceExportDeclaration?: (
    node: NamespaceExportDeclaration,
  ) => void,
  readonly 'NamespaceExportDeclaration:exit'?: (
    node: NamespaceExportDeclaration,
  ) => void,
  readonly ObjectTypePrivateField?: (node: ObjectTypePrivateField) => void,
  readonly 'ObjectTypePrivateField:exit'?: (
    node: ObjectTypePrivateField,
  ) => void,
  readonly SatisfiesExpression?: (node: SatisfiesExpression) => void,
  readonly 'SatisfiesExpression:exit'?: (node: SatisfiesExpression) => void,
  readonly TemplateLiteralTypeAnnotation?: (
    node: TemplateLiteralTypeAnnotation,
  ) => void,
  readonly 'TemplateLiteralTypeAnnotation:exit'?: (
    node: TemplateLiteralTypeAnnotation,
  ) => void,
  readonly TupleTypeElement?: (node: TupleTypeElement) => void,
  readonly 'TupleTypeElement:exit'?: (node: TupleTypeElement) => void,
}>;

export type ESQueryNodeSelectors = {
  ...ESQueryNodeSelectorsWithoutFallback,

  // We want to allow consumers to manually type their weird selectors.
  // If we use the \`ESNode\` type here then flow will error on cases like this:
  // 'FunctionDeclaration[id="foo"]'(node: FunctionDeclaration) {...}
  // But this sucks as it means someone would then have to manually do an \`if\`
  // check inside the selector body.
  readonly [selector: string]: (node: $FlowFixMe) => void,
};

export {};
