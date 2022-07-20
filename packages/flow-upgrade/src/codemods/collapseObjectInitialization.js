/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {Reference} from 'hermes-eslint';
import type {
  ESNode,
  Identifier,
  ObjectProperty,
  Statement,
} from 'hermes-estree';
import type {DetachedNode} from 'hermes-transform';
import type {Codemod} from '../Types';

import {t} from 'hermes-transform';
import {codemod} from '../Types';

export default (codemod({
  title: 'Collapse Object Initialization',
  description: 'Converts static object assignments to inline properties',
  transform: context => {
    function getStatementContainerArray(
      stmtNode: Statement,
    ): ?$ReadOnlyArray<Statement> {
      const parent = stmtNode.parent;
      switch (parent.type) {
        case 'SwitchCase': {
          return parent.consequent;
        }

        case 'BlockStatement': {
          return parent.body;
        }
        case 'Program': {
          // $FlowIgnore[incompatible-return]
          return parent.body;
        }

        default:
          return null;
      }
    }

    function getNextSibling(node: ?Statement): ?Statement {
      if (node == null) {
        return null;
      }

      const parentContainer = getStatementContainerArray(node);
      if (parentContainer == null) {
        return null;
      }

      const nodeIndex = parentContainer.indexOf(node);
      if (nodeIndex === -1) {
        return null;
      }

      return parentContainer[nodeIndex + 1];
    }

    function containsReference(
      node: ESNode,
      references: Array<Reference>,
    ): boolean {
      return references.some(reference => {
        const refRange = reference.identifier.range;
        return node.range[0] <= refRange[0] && refRange[1] <= node.range[1];
      });
    }

    type PropertyData = {
      property: DetachedNode<ObjectProperty>,
      keyName: string,
      computed: boolean,
    };

    function processStmtNode(
      idNode: Identifier,
      references: Array<Reference>,
      stmtNode: Statement,
    ):
      | {type: 'abort'}
      | {type: 'skip'}
      | {
          type: 'property',
          ...PropertyData,
        } {
      if (stmtNode.type !== 'ExpressionStatement') {
        return {type: 'abort'};
      }

      const exprNode = stmtNode.expression;
      if (exprNode.type !== 'AssignmentExpression') {
        return {type: 'abort'};
      }

      const leftNode = exprNode.left;
      if (leftNode.type !== 'MemberExpression') {
        return {type: 'abort'};
      }

      const exprRightNode = exprNode.right;
      if (containsReference(exprRightNode, references)) {
        return {type: 'abort'};
      }

      const memberObjectNode = leftNode.object;
      const memberPropertyNode = leftNode.property;
      if (
        memberObjectNode.type === 'Identifier' &&
        memberObjectNode.name === idNode.name &&
        memberPropertyNode.type === 'Identifier'
      ) {
        const shorthand: boolean =
          !leftNode.computed &&
          exprRightNode.type === 'Identifier' &&
          exprRightNode.name === memberPropertyNode.name;

        // $FlowIgnore[incompatible-type-arg] You gonna have to trust me
        const property: DetachedNode<ObjectProperty> = t.Property({
          key: memberPropertyNode,
          value: exprRightNode,
          computed: leftNode.computed,
          kind: 'init',
          method: false,
          // $FlowIgnore[incompatible-call]
          shorthand,
        });

        return {
          type: 'property',
          keyName: memberPropertyNode.name,
          property,
          computed: leftNode.computed,
        };
      }

      return {type: 'abort'};
    }

    function isWriteToObject(ref: Reference): boolean {
      let currNode: ESNode = ref.identifier;
      while (
        currNode.parent.type === 'MemberExpression' &&
        currNode.parent.object === currNode
      ) {
        currNode = currNode.parent;
      }

      return currNode.parent.type === 'AssignmentExpression';
    }

    return {
      VariableDeclarator(node) {
        const initNode = node.init;
        if (
          initNode == null ||
          initNode.type !== 'ObjectExpression' ||
          initNode.properties.length > 0
        ) {
          return;
        }

        const idNode = node.id;
        if (idNode.type !== 'Identifier' || idNode.typeAnnotation != null) {
          return;
        }

        const stmtNode = node.parent;
        if (stmtNode.type !== 'VariableDeclaration') {
          throw new Error(
            context.buildCodeFrame(
              stmtNode,
              `This is not right, parent of VariableDeclarator should always be a VariableDeclaration, instead got a "${stmtNode.type}"`,
            ),
          );
        }

        const binding = context.getBinding(idNode.name);
        if (binding == null) {
          throw new Error(
            context.buildCodeFrame(
              idNode,
              `No binding found for node of name "${idNode.name}"`,
            ),
          );
        }

        let nextStmtNode: ?Statement = stmtNode;
        let finish = false;
        const objectProperties: Array<[PropertyData, Statement]> = [];

        while (
          finish !== true &&
          (nextStmtNode = getNextSibling(nextStmtNode)) &&
          nextStmtNode != null
        ) {
          const out = processStmtNode(idNode, binding.references, nextStmtNode);
          switch (out.type) {
            case 'abort': {
              finish = true;
              break;
            }
            case 'skip': {
              break;
            }
            case 'property': {
              const {type: _type, ...rest} = out;
              objectProperties.push([rest, nextStmtNode]);
              break;
            }
          }
        }

        if (objectProperties.length > 0) {
          const assignmentRefsCount = binding.references.reduce((acc, ref) => {
            if (isWriteToObject(ref)) {
              return acc + 1;
            }
            return acc;
          }, 0);

          if (assignmentRefsCount !== objectProperties.length) {
            console.error(
              context.buildCodeFrame(
                stmtNode,
                'Object had non inlinable and inlineable properties',
              ),
            );
            // If you don't wish to codemod these mixed objects return here.
            // return;
          }

          const keyNames = new Set();
          for (const [{keyName, computed}, _] of objectProperties) {
            const name = `${computed ? 'c' : 'v'}-${keyName}`;
            if (keyNames.has(name)) {
              console.error(
                context.buildCodeFrame(
                  stmtNode,
                  `Object has duplicate key of "${keyName}"`,
                ),
              );
              return;
            }
            keyNames.add(name);
          }

          const newProperties = objectProperties.map(
            ([{property}, _]) => property,
          );
          context.replaceNode(
            initNode,
            t.ObjectExpression({properties: newProperties}),
          );
          objectProperties.forEach(([_, stmtToRemove]) => {
            context.removeStatement(stmtToRemove);
          });
        }
      },
    };
  },
}): Codemod);
