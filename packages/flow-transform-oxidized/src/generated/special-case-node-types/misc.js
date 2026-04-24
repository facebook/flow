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
  ArrowFunctionExpression as ArrowFunctionExpressionType,
  BlockStatement as BlockStatementType,
  ClassDeclaration as ClassDeclarationType,
  DeclareFunction as DeclareFunctionType,
  ESNode,
  FunctionTypeAnnotation as FunctionTypeAnnotationType,
  Identifier as IdentifierType,
  InterpreterDirective as InterpreterDirectiveType,
  Statement as StatementType,
  Token as TokenType,
  Comment as CommentType,
  TemplateElement as TemplateElementType,
  Program as ProgramType,
  DocblockMetadata as DocblockMetadataType,
  MemberExpression as MemberExpressionType,
} from 'hermes-estree';
import type {DetachedNode, MaybeDetachedNode} from '../../detachedNode';

import {
  asDetachedNode,
  detachedProps,
  setParentPointersInDirectChildren,
} from '../../detachedNode';

/*********************************************************************
 * this file should only contain one-off variant node type functions *
 * if you are creating multiple variants for the same "type", then   *
 * put them in their own file to help keep things organised          *
 *********************************************************************/

// hermes adds an `id` prop which is always null, and it adds an `expression`
// boolean which is true when the body isn't a BlockStatement.
// No need to make consumers set these
export type ArrowFunctionExpressionProps = {
  +params: $ReadOnlyArray<
    MaybeDetachedNode<ArrowFunctionExpressionType['params'][number]>,
  >,
  +body: MaybeDetachedNode<ArrowFunctionExpressionType['body']>,
  +typeParameters?: ?MaybeDetachedNode<
    ArrowFunctionExpressionType['typeParameters'],
  >,
  +returnType?: ?MaybeDetachedNode<ArrowFunctionExpressionType['returnType']>,
  +predicate?: ?MaybeDetachedNode<ArrowFunctionExpressionType['predicate']>,
  +async: ArrowFunctionExpressionType['async'],
};
export function ArrowFunctionExpression(props: {
  ...$ReadOnly<ArrowFunctionExpressionProps>,
  +parent?: ESNode,
}): DetachedNode<ArrowFunctionExpressionType> {
  const node = detachedProps<ArrowFunctionExpressionType>(props.parent, {
    type: 'ArrowFunctionExpression',
    id: null,
    // $FlowExpectedError[incompatible-use]
    expression: props.body.type !== 'BlockStatement',
    params: props.params.map(n => asDetachedNode(n)),
    body: asDetachedNode(props.body),
    // $FlowFixMe[incompatible-type]
    typeParameters: asDetachedNode(props.typeParameters),
    // $FlowFixMe[incompatible-type]
    returnType: asDetachedNode(props.returnType),
    // $FlowFixMe[incompatible-type]
    predicate: asDetachedNode(props.predicate),
    async: props.async,
  });
  setParentPointersInDirectChildren(node);
  return node;
}

export type ClassDeclarationProps = {
  +id?: ?MaybeDetachedNode<ClassDeclarationType['id']>,
  +typeParameters?: ?MaybeDetachedNode<ClassDeclarationType['typeParameters']>,
  +superClass?: ?MaybeDetachedNode<ClassDeclarationType['superClass']>,
  +superTypeArguments?: ?MaybeDetachedNode<
    ClassDeclarationType['superTypeArguments'],
  >,
  // make this optional as it's rarer that people would want to include them
  +implements?: $ReadOnlyArray<
    MaybeDetachedNode<ClassDeclarationType['implements'][number]>,
  >,
  // make this optional as it's rarer that people would want to include them
  +decorators?: $ReadOnlyArray<
    MaybeDetachedNode<ClassDeclarationType['decorators'][number]>,
  >,
  +body: MaybeDetachedNode<ClassDeclarationType['body']>,
};
export function ClassDeclaration(props: {
  ...$ReadOnly<ClassDeclarationProps>,
  +parent?: ESNode,
}): DetachedNode<ClassDeclarationType> {
  const node = detachedProps<ClassDeclarationType>(props.parent, {
    type: 'ClassDeclaration',
    // $FlowFixMe[incompatible-type]
    id: asDetachedNode(props.id),
    // $FlowFixMe[incompatible-type]
    typeParameters: asDetachedNode(props.typeParameters),
    // $FlowFixMe[incompatible-type]
    superClass: asDetachedNode(props.superClass),
    // $FlowFixMe[incompatible-type]
    superTypeArguments: asDetachedNode(props.superTypeArguments),
    decorators: (props.decorators ?? []).map(n => asDetachedNode(n)),
    implements: (props.implements ?? []).map(n => asDetachedNode(n)),
    body: asDetachedNode(props.body),
  });
  setParentPointersInDirectChildren(node);
  return node;
}

// raw/cooked are on a subobject in the estree spec, but are flat on the hermes types
export type TemplateElementProps = {
  +tail: TemplateElementType['tail'],
  +cooked: TemplateElementType['value']['cooked'],
  +raw: TemplateElementType['value']['raw'],
};
export function TemplateElement(props: {
  ...$ReadOnly<TemplateElementProps>,
  +parent?: ESNode,
}): DetachedNode<TemplateElementType> {
  return detachedProps<TemplateElementType>(props.parent, {
    type: 'TemplateElement',
    tail: props.tail,
    value: {
      cooked: props.cooked,
      raw: props.raw,
    },
  });
}

// Identifier has a bunch of stuff that usually you don't want to provide - so we have
// this manual def to allow us to default some values
export type IdentifierProps = {
  +name: IdentifierType['name'],
  +typeAnnotation?: ?MaybeDetachedNode<IdentifierType['typeAnnotation']>,
  +optional?: IdentifierType['optional'],
};
export function Identifier(props: {
  ...$ReadOnly<IdentifierProps>,
  +parent?: ESNode,
}): DetachedNode<IdentifierType> {
  const node = detachedProps<IdentifierType>(props.parent, {
    type: 'Identifier',
    name: props.name,
    optional: props.optional ?? false,
    // $FlowFixMe[incompatible-type]
    typeAnnotation: asDetachedNode(props.typeAnnotation),
  });
  setParentPointersInDirectChildren(node);
  return node;
}

// Program has a bunch of stuff that usually you don't want to provide - so we have
// this manual def to allow us to default some values
export type ProgramProps = {
  +sourceType?: ?ProgramType['sourceType'],
  +body: $ReadOnlyArray<MaybeDetachedNode<ProgramType['body'][number]>>,
  +tokens?: ?$ReadOnlyArray<MaybeDetachedNode<TokenType>>,
  +comments?: ?$ReadOnlyArray<MaybeDetachedNode<CommentType>>,
  +interpreter?: ?string,
  +docblock?: ?DocblockMetadataType,
};
export function Program(props: {
  ...$ReadOnly<ProgramProps>,
}): DetachedNode<ProgramType> {
  return detachedProps<ProgramType>(null, {
    type: 'Program',
    sourceType: props.sourceType ?? 'module',
    body: props.body.map(n => asDetachedNode(n)),
    tokens: props.tokens ?? [],
    comments: props.comments ?? [],
    interpreter:
      props.interpreter != null
        ? // $FlowFixMe[incompatible-type]
          asDetachedNode<InterpreterDirectiveType>({
            type: 'InterpreterDirective',
            value: props.interpreter,
          })
        : null,
    docblock: props.docblock,
  });
}

// the type annotation is stored on the Identifier's typeAnnotation
// which is super awkward to work with and type - so we flatten the input
// and put it in the right spot after
export type DeclareFunctionProps = {
  +name: string,
  +functionType: MaybeDetachedNode<FunctionTypeAnnotationType>,
  +predicate?: ?MaybeDetachedNode<DeclareFunctionType['predicate']>,
};
export function DeclareFunction(props: {
  ...$ReadOnly<DeclareFunctionProps>,
  +parent?: ESNode,
}): DetachedNode<DeclareFunctionType> {
  const node = detachedProps<DeclareFunctionType>(props.parent, {
    type: 'DeclareFunction',
    id: detachedProps(null, {
      type: 'Identifier',
      name: props.name,
      typeAnnotation: detachedProps(null, {
        type: 'TypeAnnotation',
        typeAnnotation: asDetachedNode(props.functionType),
      }),
    }),
    // $FlowFixMe[incompatible-type]
    predicate: asDetachedNode(props.predicate),
  });
  setParentPointersInDirectChildren(node);
  return node;
}

export type MemberExpressionProps = {
  +object: MaybeDetachedNode<MemberExpressionType['object']>,
  +property: MaybeDetachedNode<MemberExpressionType['property']>,
  +computed: MemberExpressionType['computed'],
  +optional?: MemberExpressionType['optional'],
};

export function MemberExpression(props: {
  ...$ReadOnly<MemberExpressionProps>,
  +parent?: ESNode,
}): DetachedNode<MemberExpressionType> {
  const node = detachedProps<MemberExpressionType>(props.parent, {
    type: 'MemberExpression',
    object: asDetachedNode(props.object),
    property: asDetachedNode(props.property),
    computed: props.computed,
    optional: props.optional ?? false,
  });
  setParentPointersInDirectChildren(node);
  return node;
}

// Ignore the hermes-specific `implicit` property.
export type BlockStatementProps = {
  +body: $ReadOnlyArray<MaybeDetachedNode<StatementType>>,
};
export function BlockStatement(props: {
  ...$ReadOnly<BlockStatementProps>,
  +parent?: ESNode,
}): DetachedNode<BlockStatementType> {
  const node = detachedProps<BlockStatementType>(props.parent, {
    type: 'BlockStatement',
    body: props.body.map(n => asDetachedNode(n)),
  });
  setParentPointersInDirectChildren(node);
  return node;
}
