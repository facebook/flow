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

/**
 * Transform record declarations.
 */

import type {ParserOptions} from '../ParserOptions';
import type {
  AssignmentPattern,
  ClassDeclaration,
  ClassMember,
  ESNode,
  Identifier,
  MemberExpression,
  MethodDefinition,
  NewExpression,
  ObjectExpression,
  ObjectPattern,
  ObjectPropertyKey,
  Program,
  RecordDeclaration,
  RecordDeclarationProperty,
  RecordDeclarationStaticProperty,
  RecordExpression,
  ThisExpression,
} from 'flow-estree-oxidized';

import {isBigIntLiteral} from 'flow-estree-oxidized';
import {SimpleTransform} from '../transform/SimpleTransform';
import {
  deepCloneNode,
  shallowCloneNode,
} from '../transform/astNodeMutationHelpers';
import {EMPTY_PARENT, etc, ident} from '../utils/Builders';
import isReservedWord from '../utils/isReservedWord';
import GenID from '../utils/GenID';

function nameOfKey(key: ObjectPropertyKey): string {
  switch (key.type) {
    case 'Identifier':
      return key.name;
    case 'Literal':
      if (isBigIntLiteral(key)) {
        return key.bigint;
      }
      return String(key.value);
  }
}

function mapRecordDeclaration(
  genID: GenID,
  node: RecordDeclaration,
): ClassDeclaration {
  const ownProperties: Array<RecordDeclarationProperty> = [];
  const staticProperties: Array<RecordDeclarationStaticProperty> = [];
  const methods: Array<MethodDefinition> = [];
  const staticMethods: Array<MethodDefinition> = [];

  for (const element of node.body.elements) {
    switch (element.type) {
      case 'RecordDeclarationProperty':
        ownProperties.push(element);
        break;
      case 'RecordDeclarationStaticProperty':
        staticProperties.push(element);
        break;
      case 'MethodDefinition':
        if (element.static) {
          staticMethods.push(element);
        } else {
          methods.push(element);
        }
        break;
    }
  }

  const reservedPropNames = new Map<string, string>();

  // Create constructor parameter as an object pattern with all properties
  const constructorParam: ObjectPattern = {
    type: 'ObjectPattern',
    properties: ownProperties.map(prop => {
      const {key, defaultValue} = prop;
      const keyName = nameOfKey(key);

      const getValue = (
        bindingIdent: Identifier,
      ): AssignmentPattern | Identifier =>
        defaultValue != null
          ? {
              type: 'AssignmentPattern',
              left: bindingIdent,
              right: deepCloneNode(defaultValue),
              ...etc(),
            }
          : bindingIdent;

      switch (key.type) {
        case 'Identifier': {
          const needsNewBinding = isReservedWord(keyName);
          const bindingName = needsNewBinding ? genID.id() : keyName;
          const bindingIdent = ident(bindingName);
          if (needsNewBinding) {
            reservedPropNames.set(keyName, bindingName);
          }

          if (needsNewBinding) {
            return {
              type: 'Property',
              kind: 'init',
              key: shallowCloneNode(key),
              value: getValue(bindingIdent),
              shorthand: false,
              method: false,
              computed: false,
              ...etc(),
              parent: EMPTY_PARENT,
            };
          } else {
            return {
              type: 'Property',
              kind: 'init',
              key: shallowCloneNode(key),
              value: getValue(bindingIdent),
              shorthand: true,
              method: false,
              computed: false,
              ...etc(),
              parent: EMPTY_PARENT,
            };
          }
        }
        case 'Literal': {
          const bindingName = genID.id();
          const bindingIdent = ident(bindingName);
          reservedPropNames.set(keyName, bindingName);

          return {
            type: 'Property',
            kind: 'init',
            key: shallowCloneNode(key),
            value: getValue(bindingIdent),
            shorthand: false,
            method: false,
            computed: false,
            ...etc(),
            parent: EMPTY_PARENT,
          };
        }
      }
    }),
    typeAnnotation: null,
    ...etc(),
  };

  // Create the constructor method
  const constructor: MethodDefinition = {
    type: 'MethodDefinition',
    key: ident('constructor'),
    kind: 'constructor',
    computed: false,
    static: false,
    decorators: [],
    value: {
      type: 'FunctionExpression',
      id: null,
      params: [constructorParam],
      body: {
        type: 'BlockStatement',
        body: ownProperties.map(({key}) => {
          const keyName = nameOfKey(key);
          const bindingIdent = ident(reservedPropNames.get(keyName) ?? keyName);
          const object: ThisExpression = {type: 'ThisExpression', ...etc()};
          const memberExpression: MemberExpression =
            key.type === 'Identifier'
              ? {
                  type: 'MemberExpression',
                  object,
                  property: shallowCloneNode(key),
                  computed: false,
                  optional: false,
                  ...etc(),
                }
              : {
                  type: 'MemberExpression',
                  object,
                  property: shallowCloneNode(key),
                  computed: true,
                  optional: false,
                  ...etc(),
                };
          return {
            type: 'ExpressionStatement',
            expression: {
              type: 'AssignmentExpression',
              operator: '=',
              left: memberExpression,
              right: bindingIdent,
              ...etc(),
            },
            directive: null,
            ...etc(),
          };
        }),
        ...etc(),
      },
      generator: false,
      async: false,
      predicate: null,
      returnType: null,
      typeParameters: null,
      ...etc(),
    },
    ...etc(),
    parent: EMPTY_PARENT,
  };

  const classStaticProperties: ReadonlyArray<ClassMember> =
    staticProperties.map(prop => ({
      type: 'PropertyDefinition',
      key: shallowCloneNode(prop.key),
      value: deepCloneNode(prop.value),
      static: true,
      decorators: [],
      typeAnnotation: null,
      variance: null,
      computed: false,
      declare: false,
      optional: false,
      ...etc(),
      parent: EMPTY_PARENT,
    }));

  const classBodyElements: ReadonlyArray<ClassMember> = [
    constructor,
    ...methods,
    ...classStaticProperties,
    ...staticMethods,
  ];

  return {
    type: 'ClassDeclaration',
    id: shallowCloneNode(node.id),
    body: {
      type: 'ClassBody',
      body: classBodyElements,
      ...etc(),
      parent: EMPTY_PARENT,
    },
    superClass: null,
    typeParameters: null,
    superTypeArguments: null,
    implements: [],
    decorators: [],
    ...etc(),
  };
}

function mapRecordExpression(node: RecordExpression): NewExpression {
  const obj: ObjectExpression = {
    type: 'ObjectExpression',
    properties: node.properties.properties,
    ...etc(),
  };
  return {
    type: 'NewExpression',
    callee: node.recordConstructor,
    arguments: [obj],
    typeArguments: null,
    ...etc(),
  };
}

export function transformProgram(
  program: Program,
  _options: ParserOptions,
): Program {
  const genID = new GenID('r');
  return SimpleTransform.transformProgram(program, {
    transform(node: ESNode) {
      switch (node.type) {
        case 'RecordDeclaration': {
          return mapRecordDeclaration(genID, node);
        }
        case 'RecordExpression': {
          return mapRecordExpression(node);
        }
        case 'Identifier': {
          // A rudimentary check to avoid some collisions with our generated
          // variable names. Ideally, we would have access a scope analyzer
          // inside the transform instead.
          genID.addUsage(node.name);
          return node;
        }
        default:
          return node;
      }
    },
  });
}
