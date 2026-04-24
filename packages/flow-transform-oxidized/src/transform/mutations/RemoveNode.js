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
  ComponentParameter,
  ComponentTypeParameter,
  ClassMember,
  EnumBooleanMember,
  EnumDefaultedMember,
  EnumNumberMember,
  EnumStringMember,
  FunctionParameter,
  FunctionTypeParam,
  JSXAttribute,
  ImportDeclaration,
  ObjectTypeCallProperty,
  ObjectTypeIndexer,
  ObjectTypeInternalSlot,
  ObjectTypeProperty,
  ObjectTypeSpreadProperty,
  Property,
  SpreadElement,
} from 'flow-estree-oxidized';
import type {MutationContext} from '../MutationContext';
import type {DetachedNode} from '../../detachedNode';

import {astArrayMutationHelpers} from 'flow-parser-oxidized';
import {InvalidRemovalError} from '../Errors';

export type RemoveNodeMutation = $ReadOnly<{
  type: 'removeNode',
  node:
    | ClassMember
    | ComponentParameter
    | ComponentTypeParameter
    | EnumBooleanMember
    | EnumDefaultedMember
    | EnumNumberMember
    | EnumStringMember
    | FunctionParameter
    | FunctionTypeParam
    | JSXAttribute
    | ImportDeclaration
    | ObjectTypeCallProperty
    | ObjectTypeIndexer
    | ObjectTypeInternalSlot
    | ObjectTypeProperty
    | ObjectTypeSpreadProperty
    | Property
    | SpreadElement,
}>;

export function createRemoveNodeMutation(
  node: RemoveNodeMutation['node'],
): RemoveNodeMutation {
  return {
    type: 'removeNode',
    node,
  };
}

const VALID_ENUM_MEMBER_PARENTS: $ReadOnlyArray<string> = [
  'EnumBooleanBody',
  'EnumNumberBody',
  'EnumBigIntBody',
  'EnumStringBody',
  'EnumSymbolBody',
];
const VALID_FUNCTION_PARAMETER_PARENTS: $ReadOnlyArray<string> = [
  'ArrowFunctionExpression',
  'FunctionDeclaration',
  'FunctionExpression',
];
const VALID_PROPERTY_PARENTS: $ReadOnlyArray<string> = [
  'ObjectExpression',
  'ObjectPattern',
];
const VALID_COMPONENT_TYPE_PARAMETER_PARENTS: $ReadOnlyArray<string> = [
  'DeclareComponent',
  'ComponentTypeAnnotation',
];
function getRemovalParent(node: RemoveNodeMutation['node']): $ReadOnly<{
  type: 'array',
  parent: ESNode,
  key: string,
  targetIndex: number,
}> {
  const key = ((): string => {
    function getErrorMessage(expectedParents: $ReadOnlyArray<string>): string {
      return `Tried to remove ${node.type} from parent of type ${
        node.parent.type
      }.\nHowever ${
        node.type
      } can only be safely removed from parent of type ${expectedParents.join(
        ' | ',
      )}.`;
    }
    function assertParent(expectedParents_: string | $ReadOnlyArray<string>) {
      const expectedParents =
        typeof expectedParents_ === 'string'
          ? [expectedParents_]
          : expectedParents_;
      if (!expectedParents.includes(node.parent.type)) {
        return new InvalidRemovalError(getErrorMessage(expectedParents));
      }
    }

    switch (node.type) {
      // ClassMember
      case 'PropertyDefinition':
      case 'MethodDefinition':
        assertParent('ClassBody');
        return 'body';

      case 'EnumBooleanMember':
      case 'EnumDefaultedMember':
      case 'EnumNumberMember':
      case 'EnumStringMember':
        assertParent(VALID_ENUM_MEMBER_PARENTS);
        return 'members';

      // Components
      case 'ComponentParameter':
        assertParent('ComponentDeclaration');
        return 'params';

      case 'ComponentTypeParameter':
        assertParent(VALID_COMPONENT_TYPE_PARAMETER_PARENTS);
        return 'params';

      // FunctionParameter
      case 'AssignmentPattern':
      case 'ArrayPattern':
      case 'ObjectPattern':
        assertParent(VALID_FUNCTION_PARAMETER_PARENTS);
        return 'params';

      case 'FunctionTypeParam':
        assertParent('FunctionTypeAnnotation');
        return 'params';

      case 'JSXAttribute':
        assertParent('JSXOpeningElement');
        return 'attributes';

      case 'ImportDeclaration':
        assertParent('Program');
        return 'body';

      case 'ObjectTypeCallProperty':
        assertParent('ObjectTypeAnnotation');
        return 'callProperties';

      case 'ObjectTypeIndexer':
        assertParent('ObjectTypeAnnotation');
        return 'indexers';

      case 'ObjectTypeInternalSlot':
        assertParent('ObjectTypeAnnotation');
        return 'internalSlots';

      case 'ObjectTypeProperty':
      case 'ObjectTypeSpreadProperty':
        assertParent('ObjectTypeAnnotation');
        return 'properties';

      case 'Property':
        assertParent(VALID_PROPERTY_PARENTS);
        return 'properties';

      // Identifier can be the child of a number of usecases
      case 'Identifier':
        switch (node.parent.type) {
          case 'ArrowFunctionExpression':
          case 'FunctionDeclaration':
          case 'FunctionExpression':
            return 'params';

          case 'ArrayExpression':
          case 'ArrayPattern':
            return 'elements';

          default:
            throw new InvalidRemovalError(
              getErrorMessage([
                'ArrowFunctionExpression',
                'FunctionDeclaration',
                'FunctionExpression',
                'ArrayExpression',
                'ArrayPattern',
              ]),
            );
        }

      // RestElement can be the child of a number of usecases
      case 'RestElement':
        switch (node.parent.type) {
          case 'ArrowFunctionExpression':
          case 'FunctionDeclaration':
          case 'FunctionExpression':
          case 'ComponentDeclaration':
            return 'params';

          case 'ArrayPattern':
            return 'elements';

          case 'ObjectPattern':
            return 'properties';

          // $FlowFixMe[incompatible-type]
          // $FlowFixMe[invalid-compare]
          case 'OptionalCallExpression':
          case 'CallExpression':
          case 'NewExpression':
            return 'arguments';

          default:
            throw new InvalidRemovalError(
              getErrorMessage([
                'ArrowFunctionExpression',
                'FunctionDeclaration',
                'FunctionExpression',
                'ComponentDeclaration',
                'ArrayPattern',
                'ObjectPattern',
                'CallExpression',
                'OptionalCallExpression',
                'NewExpression',
              ]),
            );
        }

      // SpreadElement can be the child of a number of usecases
      case 'SpreadElement':
        switch (node.parent.type) {
          case 'ArrayExpression':
            return 'elements';

          case 'ObjectExpression':
            return 'properties';

          // $FlowFixMe[incompatible-type]
          // $FlowFixMe[invalid-compare]
          case 'OptionalCallExpression':
          case 'CallExpression':
          case 'NewExpression':
            return 'arguments';

          default:
            throw new InvalidRemovalError(
              getErrorMessage([
                'ArrayExpression',
                'ObjectExpression',
                'CallExpression',
                'OptionalCallExpression',
                'NewExpression',
              ]),
            );
        }

      default:
        throw new InvalidRemovalError(
          `Cannot perform a remove mutation on node of type ${node.type}`,
        );
    }
  })();

  const targetIndex = (() => {
    // $FlowExpectedError[prop-missing]
    const arr = node.parent[key];
    const idx = arr.indexOf(node);
    // $FlowFixMe[invalid-compare]
    if (idx === -1) {
      throw new InvalidRemovalError(
        `Could not find target in array of \`${node.parent.type}.${key}\`.`,
      );
    }
    return idx;
  })();

  return {
    type: 'array',
    parent: node.parent,
    key,
    targetIndex,
  };
}

export function performRemoveNodeMutation(
  mutationContext: MutationContext,
  mutation: RemoveNodeMutation,
): ESNode {
  const removalParent = getRemovalParent(mutation.node);

  mutationContext.markDeletion(mutation.node);
  mutationContext.markMutation(removalParent.parent, removalParent.key);

  const parent: interface {
    [string]: $ReadOnlyArray<DetachedNode<RemoveNodeMutation['node']>>,
  } = removalParent.parent;
  parent[removalParent.key] = astArrayMutationHelpers.removeFromArray(
    parent[removalParent.key],
    removalParent.targetIndex,
  );

  return removalParent.parent;
}
