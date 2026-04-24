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
 * Transform Flow Enum declarations (https://flow.org/en/docs/enums/).
 */

import type {ParserOptions} from '../ParserOptions';
import type {
  ESNode,
  EnumDeclaration,
  ExportDefaultDeclaration,
  ObjectPropertyWithNonShorthandStaticName,
  Program,
} from 'flow-estree-oxidized';

import {isEnumDeclaration} from 'flow-estree-oxidized';
import {SimpleTransform} from '../transform/SimpleTransform';
import {
  EMPTY_PARENT,
  callExpression,
  etc,
  ident,
  stringLiteral,
  variableDeclaration,
} from '../utils/Builders';

function mapEnumDeclaration(node: EnumDeclaration, options: ParserOptions) {
  const {body} = node;
  const {members} = body;
  const getRuntime = options.transformOptions?.TransformEnumSyntax?.getRuntime;
  const enumModule =
    typeof getRuntime === 'function'
      ? getRuntime()
      : callExpression(ident('require'), [stringLiteral('flow-enums-runtime')]);
  const mirrored =
    body.type === 'EnumStringBody' &&
    (!members.length || members[0].type === 'EnumDefaultedMember');
  const enumExpression = mirrored
    ? callExpression(
        {
          type: 'MemberExpression',
          object: enumModule,
          property: ident('Mirrored'),
          computed: false,
          optional: false,
          ...etc(),
        },
        [
          {
            type: 'ArrayExpression',
            elements: members.map(member => stringLiteral(member.id.name)),
            trailingComma: false,
            ...etc(),
          },
        ],
      )
    : callExpression(enumModule, [
        {
          type: 'ObjectExpression',
          properties: members.map(
            (member): ObjectPropertyWithNonShorthandStaticName => ({
              type: 'Property',
              key: member.id,
              value:
                // String enums with `EnumDefaultedMember` are handled above by
                // calculation of `mirrored`.
                member.type === 'EnumDefaultedMember'
                  ? callExpression(ident('Symbol'), [
                      stringLiteral(member.id.name),
                    ])
                  : member.init,
              kind: 'init',
              method: false,
              shorthand: false,
              computed: false,
              ...etc(),
              parent: EMPTY_PARENT,
            }),
          ),
          ...etc(),
        },
      ]);
  return variableDeclaration('const', node.id, enumExpression);
}

export function transformProgram(
  program: Program,
  options: ParserOptions,
): Program {
  return SimpleTransform.transformProgram(program, {
    transform(node: ESNode): ESNode | $ReadOnlyArray<ESNode> {
      switch (node.type) {
        case 'EnumDeclaration': {
          return mapEnumDeclaration(node, options);
        }
        case 'ExportDefaultDeclaration': {
          const {declaration} = node;
          if (isEnumDeclaration(declaration)) {
            const enumDeclaration = mapEnumDeclaration(declaration, options);
            const exportDefault: ExportDefaultDeclaration =
              SimpleTransform.nodeWith(node, {
                declaration: ident(declaration.id.name),
              });
            return [enumDeclaration, exportDefault];
          } else {
            return node;
          }
        }
        default: {
          return node;
        }
      }
    },
  });
}
