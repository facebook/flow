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
  BaseNode,
  BaseToken,
  BlockStatement,
  CallExpression,
  ChainExpression,
  ClassPropertyNameComputed,
  ClassPropertyNameNonComputed,
  DeclareVariable,
  ESNode,
  ExportAllDeclaration,
  Expression,
  FunctionParameter,
  Identifier,
  ImportExpression,
  JSXElement,
  Literal,
  MethodDefinition,
  PrivateIdentifier,
  Program,
  Property,
  PropertyDefinition,
  RestElement,
  SpreadElement,
  StringLiteral,
  Super,
  TemplateElement,
  TypeAnnotation,
  TypeofTypeAnnotation,
  TypeParameterDeclaration,
  TypeParameterInstantiation,
  Variance,
} from 'flow-estree-oxidized';
import type {ParserOptions} from '../ParserOptions';
import type {VisitorKeys} from '../generated/ESTreeVisitorKeys';

import {SimpleTransform} from '../transform/SimpleTransform';
import {SimpleTraverser} from '../traverse/SimpleTraverser';
import FlowVisitorKeys from '../generated/ESTreeVisitorKeys';
import {createSyntaxError} from '../utils/createSyntaxError';

// Rely on the mapper to fix up parent relationships.
const EMPTY_PARENT: $FlowFixMe = null;

const FlowESTreeAndBabelVisitorKeys: VisitorKeys = {
  ...FlowVisitorKeys,
  BigIntLiteral: [],
  BlockStatement: ['directives', ...FlowVisitorKeys.BlockStatement],
  BooleanLiteral: [],
  ClassExpression: ['superTypeParameters', ...FlowVisitorKeys.ClassExpression],
  ClassDeclaration: [
    'superTypeParameters',
    ...FlowVisitorKeys.ClassDeclaration,
  ],
  ClassMethod: ['key', 'params', 'body', 'returnType', 'typeParameters'],
  ClassPrivateMethod: ['key', 'params', 'body', 'returnType', 'typeParameters'],
  ClassProperty: ['key', 'value', 'typeAnnotation', 'variance'],
  ClassPrivateProperty: ['key', 'value', 'typeAnnotation', 'variance'],
  Directive: ['value'],
  DirectiveLiteral: [],
  ExportNamespaceSpecifier: ['exported'],
  File: ['program', 'comments'],
  Import: [],
  NullLiteral: [],
  NumericLiteral: [],
  ObjectMethod: [
    ...FlowVisitorKeys.Property,
    'params',
    'body',
    'returnType',
    'typeParameters',
  ],
  ObjectProperty: FlowVisitorKeys.Property,
  OptionalCallExpression: ['callee', 'arguments', 'typeArguments'],
  OptionalMemberExpression: ['object', 'property'],
  PrivateName: ['id'],
  Program: ['directives', ...FlowVisitorKeys.Program],
  RegExpLiteral: [],
  RestElement: [...FlowVisitorKeys.RestElement, 'typeAnnotation'],
  StringLiteral: [],
  TupleTypeAnnotation: ['types'],
  CommentBlock: [],
  CommentLine: [],
};

function nodeWith<T: ESNode>(node: T, overrideProps: Partial<T>): T {
  return SimpleTransform.nodeWith(
    node,
    overrideProps,
    FlowESTreeAndBabelVisitorKeys,
  );
}

/**
 * Babel node types
 *
 * Copied from: https://github.com/babel/babel/blob/main/packages/babel-types/src/ast-types/generated/index.ts
 */
export interface BabelFile extends BaseNode {
  +type: 'File';
  +program: Program;
  +comments: $ReadOnlyArray<BabelComment>;
}

interface BabelCommentBlock extends BaseToken {
  +type: 'CommentBlock';
  +value: string;
}
interface BabelCommentLine extends BaseToken {
  +type: 'CommentLine';
  +value: string;
}
type BabelComment = BabelCommentBlock | BabelCommentLine;

interface BabelObjectMethod extends BaseNode {
  +type: 'ObjectMethod';
  +kind: 'method' | 'get' | 'set';
  +key: Expression;
  +params: $ReadOnlyArray<FunctionParameter>;
  +body: BlockStatement;
  +computed: boolean;
  +generator: boolean;
  +async: boolean;
  +returnType?: TypeAnnotation | null;
  +typeParameters?: TypeParameterDeclaration | null;
  +variance?: boolean | null;

  // TODO: Should these exist?
  // +id: ...,
  // +value: Expression;
  // +method: boolean;
  // +shorthand: boolean;
}

interface BabelObjectProperty extends BaseNode {
  +type: 'ObjectProperty';
  +computed: boolean;
  +key: Expression;
  +value: Expression;
  +method: boolean;
  +shorthand: boolean;
}

interface BabelClassMethodBase extends BaseNode {
  +kind: 'get' | 'set' | 'method' | 'constructor';
  +computed: boolean;
  +static: boolean;
  +key:
    | Identifier
    | StringLiteral
    | ClassPropertyNameComputed
    | ClassPropertyNameNonComputed;
  +id: null;
  +params: $ReadOnlyArray<FunctionParameter>;
  +body: BlockStatement;
  +async: boolean;
  +generator: boolean;
  +returnType?: TypeAnnotation | null;
  +typeParameters?: TypeParameterDeclaration | null;
  +predicate: null;
}

interface BabelClassPrivateMethod extends BabelClassMethodBase {
  +type: 'ClassPrivateMethod';
}
interface BabelClassMethod extends BabelClassMethodBase {
  +type: 'ClassMethod';
}

interface BabelClassProperty extends BaseNode {
  +type: 'ClassProperty';
  +key: Expression;
  +value: null | Expression;
  +typeAnnotation: null | TypeAnnotation;
  +static: boolean;
  +variance: null | Variance;
  +declare: boolean;
  +optional: null | boolean;
  +computed: boolean;
}

interface BabelClassPrivateProperty extends BaseNode {
  +type: 'ClassPrivateProperty';
  +key: BabelPrivateName;
  +value: null | Expression;
  +typeAnnotation: null | TypeAnnotation;
  +static: boolean;
  +variance: null | Variance;
}

interface BabelExportNamespaceSpecifier extends BaseNode {
  +type: 'ExportNamespaceSpecifier';
  +exported: Identifier;
}

interface BabelExportNamedDeclaration extends BaseNode {
  +type: 'ExportNamedDeclaration';
  +declaration?: null;
  +specifiers: $ReadOnlyArray<BabelExportNamespaceSpecifier>;
  +source?: StringLiteral | null;
  +exportKind: 'value' | 'type';
}

interface BabelPrivateName extends BaseNode {
  +type: 'PrivateName';
  +id: Identifier;
}

interface BabelOptionalMemberExpression extends BaseNode {
  +type: 'OptionalMemberExpression';
  +object:
    | Expression
    | Super
    | BabelOptionalMemberExpression
    | BabelOptionalCallExpression;
  +property: Expression | Identifier | PrivateIdentifier;
  +computed: boolean;
  +optional: boolean;
}

interface BabelOptionalCallExpression extends BaseNode {
  +type: 'OptionalCallExpression';
  +callee:
    | Expression
    | Super
    | BabelOptionalMemberExpression
    | BabelOptionalCallExpression;
  +arguments: $ReadOnlyArray<Expression | SpreadElement>;
  +optional: boolean;
  +typeArguments?: TypeParameterInstantiation | null;
}

interface BabelStringLiteral extends BaseNode {
  +type: 'StringLiteral';
  +value: string;
  +extra: $ReadOnly<{
    +rawValue: string,
    +raw: string,
  }>;
}

interface BabelNumericLiteral extends BaseNode {
  +type: 'NumericLiteral';
  +value: number;
  +extra: $ReadOnly<{
    +rawValue: number,
    +raw: string,
  }>;
}

interface BabelBigIntLiteral extends BaseNode {
  +type: 'BigIntLiteral';
  +value: string;
  +extra: $ReadOnly<{
    +rawValue: string,
    +raw: string,
  }>;
}

interface BabelBooleanLiteral extends BaseNode {
  +type: 'BooleanLiteral';
  +value: boolean;
}

interface BabelNullLiteral extends BaseNode {
  +type: 'NullLiteral';
}

interface BabelRegExpLiteral extends BaseNode {
  +type: 'RegExpLiteral';
  +extra: $ReadOnly<{
    +raw: string,
  }>;
  +pattern: string;
  +flags: string;
}

type BabelLiteral =
  | BabelStringLiteral
  | BabelNumericLiteral
  | BabelBooleanLiteral
  | BabelNullLiteral
  | BabelRegExpLiteral
  | BabelBigIntLiteral;

export type ESNodeOrBabelNode =
  | ESNode
  | BabelFile
  | BabelObjectMethod
  | BabelObjectProperty
  | BabelClassPrivateMethod
  | BabelClassMethod
  | BabelClassProperty
  | BabelClassPrivateProperty
  | BabelExportNamespaceSpecifier
  | BabelExportNamedDeclaration
  | BabelPrivateName
  | BabelOptionalMemberExpression
  | BabelOptionalCallExpression
  | BabelLiteral
  | BabelComment;

function fixSourceLocation(
  node: ESNodeOrBabelNode,
  _options: ParserOptions,
): void {
  const loc = node.loc;
  if (loc == null) {
    return;
  }

  // $FlowExpectedError[cannot-write]
  node.loc = {
    start: loc.start,
    end: loc.end,
  };

  if (node.type === 'Identifier') {
    // $FlowExpectedError[prop-missing]
    node.loc.identifierName = node.name;
  }

  // $FlowExpectedError[prop-missing]
  node.start = node.range[0];
  // $FlowExpectedError[prop-missing]
  node.end = node.range[1];

  // $FlowExpectedError[cannot-write]
  delete node.range;

  // $FlowExpectedError[cannot-write]
  // $FlowExpectedError[prop-missing]
  delete node.parent;
}

function mapNodeWithDirectives<T: Program | BlockStatement>(node: T): T {
  // $FlowExpectedError[prop-missing]
  if (node.directives != null) {
    return node;
  }

  const directives = [];
  for (const child of node.body) {
    if (child.type === 'ExpressionStatement' && child.directive != null) {
      // Construct Directive node with DirectiveLiteral value
      directives.push({
        type: 'Directive',
        value: {
          type: 'DirectiveLiteral',
          value: child.directive,
          extra: {
            rawValue: child.directive,
            raw:
              child.expression.type === 'Literal' ? child.expression.raw : '',
          },
          loc: child.expression.loc,
          range: child.expression.range,
          parent: EMPTY_PARENT,
        },
        loc: child.loc,
        range: child.range,
        parent: node,
      });
    } else {
      // Once we have found the first non-directive node we know there cannot be any more directives
      break;
    }
  }

  // Move directives from body to new directives array
  // $FlowExpectedError[incompatible-type] We are adding properties for babel that don't exist in the ESTree types.
  return nodeWith(node, {
    directives,
    body:
      directives.length === 0 ? node.body : node.body.slice(directives.length),
  });
}

function mapProgram(node: Program): BabelFile {
  // Visit child nodes and convert to directives
  const program = mapNodeWithDirectives(node);

  // Adjust start loc to beginning of file
  const startLoc = {line: 1, column: 0};

  // Adjust end loc to include last comment if program ends with a comment
  let endLoc = program.loc.end;
  let endRange = program.range[1];
  if (program.comments.length > 0) {
    const lastComment = program.comments[program.comments.length - 1];
    if (lastComment.range[1] > endRange) {
      endLoc = lastComment.loc.end;
      endRange = lastComment.range[1];
    }
  }

  const loc = {
    start: startLoc,
    end: endLoc,
  };
  const range = [0, endRange];

  const babelComments: $ReadOnlyArray<BabelComment> = program.comments.map(
    comment => {
      switch (comment.type) {
        case 'Line': {
          return {
            type: 'CommentLine',
            value: comment.value,
            loc: comment.loc,
            range: comment.range,
          };
        }
        case 'Block': {
          return {
            type: 'CommentBlock',
            value: comment.value,
            loc: comment.loc,
            range: comment.range,
          };
        }
      }
    },
  );

  // Rename root node to File node and move Program node under program property
  return {
    type: 'File',
    // $FlowExpectedError[incompatible-type] Comments, docblock and tokens are purposely missing to match the Babel AST.
    program: {
      type: 'Program',
      body: program.body,
      // $FlowExpectedError[prop-missing] This is added by `mapNodeWithDirectives`
      directives: program.directives ?? [],
      sourceType: program.sourceType,
      interpreter: program.interpreter,
      // TODO: Add back for BABEL?
      // sourceFile: options.sourceFilename,
      loc,
      range,
      parent: EMPTY_PARENT,
    },
    comments: babelComments,
    loc,
    range,
    parent: EMPTY_PARENT,
  };
}

function mapTemplateElement(node: TemplateElement): TemplateElement {
  // Adjust start loc to exclude "`" at beginning of template literal if this is the first quasi,
  // otherwise exclude "}" from previous expression.
  const startCharsToExclude = 1;

  // Adjust end loc to exclude "`" at end of template literal if this is the last quasi,
  // otherwise exclude "${" from next expression.
  const endCharsToExclude = node.tail ? 1 : 2;

  // Mutate the existing node to use the new location information.
  // NOTE: because we don't add any new properties here we cannot detect if this change has been
  //       made, so its important we don't return a new node other wise it will recursive infinitely.

  // $FlowExpectedError[cannot-write]
  node.loc = {
    start: {
      line: node.loc.start.line,
      column: node.loc.start.column + startCharsToExclude,
    },
    end: {
      line: node.loc.end.line,
      column: node.loc.end.column - endCharsToExclude,
    },
  };

  // $FlowExpectedError[cannot-write]
  node.range = [
    node.range[0] + startCharsToExclude,
    node.range[1] - endCharsToExclude,
  ];

  return node;
}

function mapProperty(node: Property): BabelObjectMethod | BabelObjectProperty {
  const key = node.key;
  const value = node.value;

  // Convert methods, getters, and setters to ObjectMethod nodes
  if (node.method || node.kind !== 'init') {
    if (value.type !== 'FunctionExpression') {
      throw createSyntaxError(
        node,
        `Invalid method property, the value must be a "FunctionExpression" or "ArrowFunctionExpression. Instead got "${value.type}".`,
      );
    }
    // Properties under the FunctionExpression value that should be moved
    // to the ObjectMethod node itself.
    const newNode: BabelObjectMethod = {
      type: 'ObjectMethod',
      // Non getter or setter methods have `kind = method`
      kind: node.kind === 'init' ? 'method' : node.kind,
      method: node.kind === 'init' ? true : false,
      computed: node.computed,
      key: key,
      id: null,
      params: value.params,
      body: value.body,
      async: value.async,
      generator: value.generator,
      returnType: value.returnType,
      typeParameters: value.typeParameters,
      loc: node.loc,
      range: node.range,
      parent: node.parent,
    };
    if (node.kind !== 'init') {
      // babel emits an empty variance property on accessors for some reason
      // $FlowExpectedError[cannot-write]
      newNode.variance = null;
    }
    return newNode;
  }

  // Non-method property nodes should be renamed to ObjectProperty
  return {
    type: 'ObjectProperty',
    computed: node.computed,
    key: node.key,
    // $FlowExpectedError[incompatible-type]
    value: (node.value: Expression),
    method: node.method,
    shorthand: node.shorthand,
    loc: node.loc,
    range: node.range,
    parent: node.parent,
  };
}

function mapMethodDefinition(
  node: MethodDefinition,
): BabelClassPrivateMethod | BabelClassMethod {
  const value = node.value;

  const BaseClassMethod = {
    kind: node.kind,
    computed: node.computed,
    static: node.static,
    key: node.key,
    id: null,

    // Properties under the FunctionExpression value that should be moved
    // to the ClassMethod node itself.
    params: value.params,
    body: value.body,
    async: value.async,
    generator: value.generator,
    returnType: value.returnType,
    typeParameters: value.typeParameters,
    predicate: null,

    loc: node.loc,
    range: node.range,
    parent: node.parent,
  };

  if (node.key.type === 'PrivateIdentifier') {
    return {
      ...BaseClassMethod,
      type: 'ClassPrivateMethod',
    };
  }

  return {
    ...BaseClassMethod,
    type: 'ClassMethod',
  };
}

function mapExportAllDeclaration(
  node: ExportAllDeclaration,
): ExportAllDeclaration | BabelExportNamedDeclaration {
  if (node.exported != null) {
    return {
      type: 'ExportNamedDeclaration',
      declaration: null,
      specifiers: [
        {
          type: 'ExportNamespaceSpecifier',
          exported: node.exported,

          // The hermes AST emits the location as the location of the entire export
          // but babel emits the location as *just* the "* as id" bit.
          // The end will always align with the end of the identifier (ezpz)
          // but the start will align with the "*" token - which we can't recover from just the AST
          // so we just fudge the start location a bit to get it "good enough"
          // it will be wrong if the AST is anything like "export      * as x from 'y'"... but oh well
          loc: {
            start: {
              column: node.loc.start.column + 'export '.length,
              line: node.loc.start.line,
            },
            end: node.exported.loc.end,
          },
          range: [node.range[0] + 'export '.length, node.exported.range[1]],
          parent: EMPTY_PARENT,
        },
      ],
      source: node.source,
      exportKind: node.exportKind,
      loc: node.loc,
      range: node.range,
      parent: node.parent,
    };
  }

  // $FlowExpectedError[cannot-write]
  delete node.exported;

  return node;
}

function mapRestElement(node: RestElement): RestElement {
  // ESTree puts type annotations on rest elements on the argument node,
  // but Babel expects type annotations on the rest element node itself.
  const argument = node.argument;
  if (
    (argument.type === 'Identifier' ||
      argument.type === 'ObjectPattern' ||
      argument.type === 'ArrayPattern') &&
    argument.typeAnnotation != null
  ) {
    // Unfortunately there's no way for us to recover the end location of
    // the argument for the general case
    const argumentRange = argument.range;
    let argumentLoc = argument.loc;
    if (argument.type === 'Identifier') {
      argumentRange[1] = argumentRange[0] + argument.name.length;
      argumentLoc = {
        start: argumentLoc.start,
        end: {
          line: argumentLoc.start.line,
          column: argumentLoc.start.column + argument.name.length,
        },
      };
    }
    return nodeWith(node, {
      typeAnnotation: argument.typeAnnotation,
      argument: nodeWith(argument, {
        typeAnnotation: null,
        range: argumentRange,
        loc: argumentLoc,
      }),
    });
  }

  return node;
}

function mapImportExpression(node: ImportExpression): CallExpression {
  // Babel expects ImportExpression to be structured as a regular
  // CallExpression where the callee is an Import node.

  // $FlowExpectedError[incompatible-type] optional and typeArguments are missing to match existing output.
  return {
    type: 'CallExpression',
    // $FlowExpectedError[incompatible-type] This is a babel specific node
    callee: {
      type: 'Import',
      loc: {
        start: node.loc.start,
        end: {
          line: node.loc.start.line,
          column: node.loc.start.column + 'import'.length,
        },
      },
      range: [node.range[0], node.range[0] + 'import'.length],
      parent: EMPTY_PARENT,
    },
    arguments: [node.source],
    loc: node.loc,
    range: node.range,
    parent: node.parent,
  };
}

function mapPrivateIdentifier(node: PrivateIdentifier): BabelPrivateName {
  return {
    type: 'PrivateName',
    id: {
      type: 'Identifier',
      name: node.name,
      optional: false,
      typeAnnotation: null,
      loc: {
        start: {
          line: node.loc.start.line,
          // babel doesn't include the hash in the identifier
          column: node.loc.start.column + 1,
        },
        end: node.loc.end,
      },
      range: [node.range[0] + 1, node.range[1]],
      parent: EMPTY_PARENT,
    },
    loc: node.loc,
    range: node.range,
    parent: node.parent,
  };
}

function mapPropertyDefinition(
  node: PropertyDefinition,
): BabelClassProperty | BabelClassPrivateProperty {
  if (node.key.type === 'PrivateIdentifier') {
    return {
      type: 'ClassPrivateProperty',
      key: mapPrivateIdentifier(node.key),
      value: node.value,
      typeAnnotation: node.typeAnnotation,
      static: node.static,
      variance: node.variance,
      loc: node.loc,
      range: node.range,
      parent: node.parent,
    };
  }

  return {
    type: 'ClassProperty',
    key: node.key,
    value: node.value,
    typeAnnotation: node.typeAnnotation,
    static: node.static,
    variance: node.variance,
    declare: node.declare,
    optional: node.optional,
    computed: node.computed,
    loc: node.loc,
    range: node.range,
    parent: node.parent,
  };
}

function mapTypeofTypeAnnotation(
  node: TypeofTypeAnnotation,
): TypeofTypeAnnotation {
  // $FlowExpectedError[cannot-write]
  delete node.typeArguments;
  // $FlowFixMe[incompatible-type]
  // $FlowFixMe[invalid-compare]
  if (node.argument.type !== 'GenericTypeAnnotation') {
    return nodeWith(node, {
      // $FlowExpectedError[incompatible-type] Special override for Babel
      argument: {
        type: 'GenericTypeAnnotation',
        id: node.argument,
        typeParameters: null,
        loc: node.argument.loc,
        range: node.argument.range,
        parent: EMPTY_PARENT,
      },
    });
  }
  return node;
}

function mapDeclareVariable(node: DeclareVariable): DeclareVariable {
  if (node.kind != null) {
    // $FlowExpectedError[cannot-write]
    delete node.kind;
  }
  return node;
}

function mapJSXElement(node: JSXElement): JSXElement {
  if (node.openingElement.typeArguments != null) {
    // $FlowExpectedError[cannot-write]
    delete node.openingElement.typeArguments;
  }

  return node;
}

type ChainExpressionReturnType =
  | BabelOptionalMemberExpression
  | BabelOptionalCallExpression
  | Expression
  | Super;

function mapChainExpressionInnerNode(
  node: Expression | Super,
): ChainExpressionReturnType {
  switch (node.type) {
    case 'MemberExpression': {
      const innerObject = mapChainExpressionInnerNode(node.object);
      if (
        !node.optional &&
        innerObject.type !== 'OptionalMemberExpression' &&
        innerObject.type !== 'OptionalCallExpression'
      ) {
        return node;
      }

      return {
        type: 'OptionalMemberExpression',
        object: innerObject,
        property: node.property,
        computed: node.computed,
        optional: node.optional,
        loc: node.loc,
        range: node.range,
        parent: node.parent,
      };
    }
    case 'CallExpression': {
      const innerCallee = mapChainExpressionInnerNode(node.callee);
      if (
        !node.optional &&
        innerCallee.type !== 'OptionalMemberExpression' &&
        innerCallee.type !== 'OptionalCallExpression'
      ) {
        return node;
      }

      return {
        type: 'OptionalCallExpression',
        callee: innerCallee,
        optional: node.optional,
        arguments: node.arguments,
        typeArguments: node.typeArguments,
        loc: node.loc,
        range: node.range,
        parent: node.parent,
      };
    }
    default: {
      return node;
    }
  }
}

function mapChainExpression(node: ChainExpression): ChainExpressionReturnType {
  return mapChainExpressionInnerNode(node.expression);
}

function mapLiteral(node: Literal): BabelLiteral {
  const base = {
    loc: node.loc,
    range: node.range,
    parent: node.parent,
  };
  switch (node.literalType) {
    case 'string': {
      return {
        type: 'StringLiteral',
        value: node.value,
        extra: {
          rawValue: node.value,
          raw: node.raw,
        },
        ...base,
      };
    }
    case 'numeric': {
      return {
        type: 'NumericLiteral',
        value: node.value,
        extra: {
          rawValue: node.value,
          raw: node.raw,
        },
        ...base,
      };
    }
    case 'bigint': {
      return {
        type: 'BigIntLiteral',
        value: node.bigint,
        extra: {
          rawValue: node.bigint,
          raw: node.raw,
        },
        ...base,
      };
    }
    case 'boolean': {
      return {
        type: 'BooleanLiteral',
        value: node.value,
        ...base,
      };
    }
    case 'null': {
      return {
        type: 'NullLiteral',
        ...base,
      };
    }
    case 'regexp': {
      return {
        type: 'RegExpLiteral',
        extra: {
          raw: node.raw,
        },
        pattern: node.regex.pattern,
        flags: node.regex.flags,
        ...base,
      };
    }
  }
}

function transformNode(node: ESNodeOrBabelNode): ESNodeOrBabelNode | null {
  switch (node.type) {
    case 'Program': {
      // Check if we have already processed this node.
      // $FlowFixMe[incompatible-type]
      // $FlowFixMe[invalid-compare]
      if (node.parent?.type === 'File') {
        return node;
      }
      return mapProgram(node);
    }
    case 'BlockStatement': {
      return mapNodeWithDirectives(node);
    }
    case 'Property': {
      return mapProperty(node);
    }
    case 'TemplateElement': {
      return mapTemplateElement(node);
    }
    case 'MethodDefinition': {
      return mapMethodDefinition(node);
    }
    case 'ExportAllDeclaration': {
      return mapExportAllDeclaration(node);
    }
    case 'RestElement': {
      return mapRestElement(node);
    }
    case 'ImportExpression': {
      return mapImportExpression(node);
    }
    case 'PrivateIdentifier': {
      return mapPrivateIdentifier(node);
    }
    case 'PropertyDefinition': {
      return mapPropertyDefinition(node);
    }
    case 'TypeofTypeAnnotation': {
      return mapTypeofTypeAnnotation(node);
    }
    case 'DeclareVariable': {
      return mapDeclareVariable(node);
    }
    case 'JSXElement': {
      return mapJSXElement(node);
    }
    case 'Literal': {
      return mapLiteral(node);
    }
    case 'ChainExpression': {
      return mapChainExpression(node);
    }
    case 'TypeCastExpression': {
      // Babel uses different positions for TypeCastExpression locations.

      // $FlowExpectedError[cannot-write]
      node.loc.start.column = node.loc.start.column + 1;
      // $FlowExpectedError[cannot-write]
      node.loc.end.column = node.loc.end.column - 1;
      // $FlowExpectedError[cannot-write]
      node.range = [node.range[0] + 1, node.range[1] - 1];
      return node;
    }
    case 'AsExpression': {
      const {typeAnnotation} = node;
      // $FlowExpectedError[cannot-write]
      node.type = 'TypeCastExpression';
      // $FlowExpectedError[cannot-write]
      node.typeAnnotation = {
        type: 'TypeAnnotation',
        typeAnnotation,
        loc: typeAnnotation.loc,
        range: typeAnnotation.range,
      };
      return node;
    }
    case 'AsConstExpression': {
      return node.expression;
    }

    /**
     * Babel has a different format for Literals
     */
    case 'NumberLiteralTypeAnnotation': {
      // $FlowExpectedError[prop-missing]
      node.extra = {
        rawValue: node.value,
        raw: node.raw,
      };
      // $FlowExpectedError[cannot-write]
      delete node.raw;
      return node;
    }
    case 'StringLiteralTypeAnnotation': {
      // $FlowExpectedError[prop-missing]
      node.extra = {
        rawValue: node.value,
        raw: node.raw,
      };
      // $FlowExpectedError[cannot-write]
      delete node.raw;
      return node;
    }
    case 'BigIntLiteralTypeAnnotation': {
      // $FlowExpectedError[prop-missing]
      node.extra = {
        rawValue: node.bigint,
        raw: node.raw,
      };
      // $FlowExpectedError[cannot-write]
      delete node.bigint;
      // $FlowExpectedError[cannot-write]
      delete node.raw;
      return node;
    }
    case 'BooleanLiteralTypeAnnotation': {
      // $FlowExpectedError[cannot-write]
      delete node.raw;
      return node;
    }
    case 'TupleTypeAnnotation': {
      // $FlowExpectedError[cannot-write]
      delete node.inexact;
      // $FlowExpectedError[prop-missing]
      node.types = node.elementTypes;
      // $FlowExpectedError[cannot-write]
      delete node.elementTypes;
      return node;
    }

    case 'UnknownTypeAnnotation': {
      return {
        type: 'GenericTypeAnnotation',
        id: {
          type: 'Identifier',
          name: 'unknown',
          optional: false,
          typeAnnotation: null,
          loc: node.loc,
          range: node.range,
          parent: EMPTY_PARENT,
        },
        typeParameters: null,
        loc: node.loc,
        range: node.range,
        parent: EMPTY_PARENT,
      };
    }
    case 'NeverTypeAnnotation': {
      return {
        type: 'GenericTypeAnnotation',
        id: {
          type: 'Identifier',
          name: 'never',
          optional: false,
          typeAnnotation: null,
          loc: node.loc,
          range: node.range,
          parent: EMPTY_PARENT,
        },
        typeParameters: null,
        loc: node.loc,
        range: node.range,
        parent: EMPTY_PARENT,
      };
    }
    case 'UndefinedTypeAnnotation': {
      return {
        type: 'GenericTypeAnnotation',
        id: {
          type: 'Identifier',
          name: 'undefined',
          optional: false,
          typeAnnotation: null,
          loc: node.loc,
          range: node.range,
          parent: EMPTY_PARENT,
        },
        typeParameters: null,
        loc: node.loc,
        range: node.range,
        parent: EMPTY_PARENT,
      };
    }

    case 'JSXText': {
      // $FlowExpectedError[prop-missing]
      node.extra = {
        rawValue: node.value,
        raw: node.raw,
      };
      // $FlowExpectedError[cannot-write]
      delete node.raw;
      return node;
    }

    /**
     * Babel condenses there output AST by removing some "falsy" values.
     */
    case 'Identifier': {
      // Babel only has these values if they are "set"
      if (node.optional === false || node.optional == null) {
        // $FlowExpectedError[cannot-write]
        delete node.optional;
      }
      if (node.typeAnnotation == null) {
        // $FlowExpectedError[cannot-write]
        delete node.typeAnnotation;
      }

      return node;
    }
    case 'CallExpression': {
      if (node.optional === false) {
        // $FlowExpectedError[cannot-write]
        delete node.optional;
      }
      if (node.typeArguments == null) {
        // $FlowExpectedError[cannot-write]
        delete node.typeArguments;
      }
      return node;
    }
    case 'OptionalCallExpression': {
      if (node.typeArguments == null) {
        // $FlowExpectedError[cannot-write]
        delete node.typeArguments;
      }
      return node;
    }
    case 'MemberExpression': {
      // $FlowExpectedError[cannot-write]
      delete node.optional;
      return node;
    }
    case 'ExpressionStatement': {
      // $FlowExpectedError[cannot-write]
      delete node.directive;
      return node;
    }
    case 'ObjectPattern':
    case 'ArrayPattern': {
      if (node.typeAnnotation == null) {
        // $FlowExpectedError[cannot-write]
        delete node.typeAnnotation;
      }
      return node;
    }
    case 'FunctionDeclaration':
    case 'FunctionExpression':
    case 'ArrowFunctionExpression':
    case 'ClassMethod': {
      // $FlowExpectedError[prop-missing]
      // $FlowExpectedError[cannot-write]
      delete node.expression;
      if (node.predicate == null) {
        // $FlowExpectedError[cannot-write]
        delete node.predicate;
      }
      if (node.returnType == null) {
        // $FlowExpectedError[cannot-write]
        delete node.returnType;
      }
      if (node.typeParameters == null) {
        // $FlowExpectedError[cannot-write]
        delete node.typeParameters;
      }
      return node;
    }
    case 'ObjectMethod': {
      if (node.returnType == null) {
        // $FlowExpectedError[cannot-write]
        delete node.returnType;
      }
      if (node.typeParameters == null) {
        // $FlowExpectedError[cannot-write]
        delete node.typeParameters;
      }
      return node;
    }
    case 'ClassPrivateMethod': {
      if (node.computed === false) {
        // $FlowExpectedError[cannot-write]
        delete node.computed;
      }
      if (node.predicate == null) {
        // $FlowExpectedError[cannot-write]
        delete node.predicate;
      }
      if (node.returnType == null) {
        // $FlowExpectedError[cannot-write]
        delete node.returnType;
      }
      if (node.typeParameters == null) {
        // $FlowExpectedError[cannot-write]
        delete node.typeParameters;
      }
      return node;
    }
    case 'ClassExpression':
    case 'ClassDeclaration': {
      if (node.decorators == null || node.decorators.length === 0) {
        // $FlowExpectedError[cannot-write]
        delete node.decorators;
      }
      if (node.implements == null || node.implements.length === 0) {
        // $FlowExpectedError[cannot-write]
        delete node.implements;
      }
      if (node.superTypeArguments == null) {
        // $FlowExpectedError[cannot-write]
        delete node.superTypeArguments;
      } else {
        // $FlowExpectedError[cannot-write]
        // $FlowExpectedError[prop-missing]
        node.superTypeParameters = node.superTypeArguments;
        // $FlowExpectedError[cannot-write]
        delete node.superTypeArguments;
      }
      if (node.typeParameters == null) {
        // $FlowExpectedError[cannot-write]
        delete node.typeParameters;
      }
      return node;
    }
    case 'ClassProperty': {
      if (node.optional === false) {
        // $FlowExpectedError[cannot-write]
        delete node.optional;
      }
      if (node.declare === false) {
        // $FlowExpectedError[cannot-write]
        delete node.declare;
      }
      if (node.typeAnnotation == null) {
        // $FlowExpectedError[cannot-write]
        delete node.typeAnnotation;
      }
      return node;
    }
    case 'ClassPrivateProperty': {
      if (node.typeAnnotation == null) {
        // $FlowExpectedError[cannot-write]
        delete node.typeAnnotation;
      }
      return node;
    }
    case 'ExportNamedDeclaration': {
      if (node.declaration == null) {
        // $FlowExpectedError[cannot-write]
        delete node.declaration;
      }
      return node;
    }
    case 'ImportDeclaration': {
      if (node.attributes == null || node.attributes.length === 0) {
        // $FlowExpectedError[cannot-write]
        delete node.attributes;
      }
      return node;
    }
    case 'ArrayExpression': {
      // $FlowExpectedError[cannot-write]
      delete node.trailingComma;
      return node;
    }
    case 'JSXOpeningElement': {
      if (node.typeArguments == null) {
        // $FlowExpectedError[cannot-write]
        delete node.typeArguments;
      }
      return node;
    }
    case 'DeclareOpaqueType':
    case 'OpaqueType': {
      if (node.lowerBound != null) {
        // $FlowExpectedError[cannot-write]
        delete node.lowerBound;
      }
      if (node.upperBound != null) {
        // $FlowExpectedError[cannot-write]
        delete node.upperBound;
      }
      return node;
    }
    default: {
      return node;
    }
  }
}

export function transformProgram(
  program: Program,
  options: ParserOptions,
): BabelFile {
  const resultNode = SimpleTransform.transform(program, {
    transform(node) {
      // $FlowExpectedError[incompatible-type] We override the type to support the additional Babel types
      return transformNode(node);
    },
    visitorKeys: FlowESTreeAndBabelVisitorKeys,
  });

  // $FlowExpectedError[incompatible-type] We override the type to support the additional Babel types
  SimpleTraverser.traverse(resultNode, {
    enter(node) {
      fixSourceLocation(node, options);
    },
    leave() {},
    visitorKeys: FlowESTreeAndBabelVisitorKeys,
  });

  // $FlowFixMe[incompatible-type]
  // $FlowFixMe[invalid-compare]
  if (resultNode?.type === 'File') {
    return resultNode;
  }
  throw new Error(
    `Unknown AST node of type "${
      resultNode?.type ?? 'NULL'
    }" returned from Babel conversion`,
  );
}
