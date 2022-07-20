/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {Codemod} from '../Types';
import type {
  ClassDeclaration,
  ClassExpression,
  ClassMember,
  MemberExpression,
} from 'hermes-estree';

import {getClassMemberName} from '../codemodUtils/getClassMemberName';
import {replaceMethodWithArrowProp} from '../codemodUtils/replaceMethodWithArrowProp';
import {codemod} from '../Types';

type MemberKind = 'Property' | 'Getter' | 'Setter' | 'Method';
type Members =
  | {
      kind: 'GetterSetter',
      getter: ClassMember,
      setter: ClassMember,
    }
  | {
      kind: MemberKind,
      member: ClassMember,
    };

function getKind(member: ClassMember): MemberKind {
  switch (member.type) {
    case 'MethodDefinition':
      switch (member.kind) {
        case 'get':
          return 'Getter';

        case 'set':
          return 'Setter';

        case 'method':
        case 'constructor':
          return 'Method';
      }
      throw new Error(`Unexpected method kind: ${member.kind}`);

    case 'PropertyDefinition':
      return 'Property';
  }

  throw new Error(`Unexpected member type: ${member.type}`);
}

function findDuplicates(
  membersMap: $ReadOnlyMap<string, Array<$ReadOnly<ClassMember>>>,
) {
  const toDelete: Array<ClassMember> = [];
  for (const [, members] of membersMap) {
    if (members.length === 1) {
      continue;
    }

    let lastMember: ?Members = null;
    for (const member of members) {
      const kind = getKind(member);
      if (lastMember == null) {
        lastMember = {kind, member};
        continue;
      }

      // handle the valid cases
      switch (lastMember.kind) {
        case 'GetterSetter':
          if (kind === 'Getter') {
            const oldGetter = lastMember.getter;
            lastMember.getter = member;
            toDelete.push(oldGetter);
            continue;
          } else if (kind === 'Setter') {
            const oldSetter = lastMember.setter;
            lastMember.setter = member;
            toDelete.push(oldSetter);
            continue;
          }
          break;

        case 'Getter':
          if (kind === 'Setter') {
            lastMember = {
              kind: 'GetterSetter',
              getter: lastMember.member,
              setter: member,
            };
            continue;
          }
          break;

        case 'Setter':
          if (kind === 'Getter') {
            lastMember = {
              kind: 'GetterSetter',
              getter: member,
              setter: lastMember.member,
            };
            continue;
          }
          break;
      }

      // overwrite and delete the old member as it's incompatible
      if (lastMember.kind === 'GetterSetter') {
        toDelete.push(lastMember.getter);
        toDelete.push(lastMember.setter);
      } else {
        toDelete.push(lastMember.member);
      }
      lastMember = {kind, member};
    }
  }

  return toDelete;
}

export default (codemod({
  title: 'Remove Duplicate Class Properties',
  description:
    'Removes useless duplicate class properties and fixes bad constructor binding in those classes',
  transform: context => {
    type InstanceMember = $ReadOnly<{
      kind: MemberKind,
      member: ClassMember,
    }>;
    type ClassStack = $ReadOnly<{
      current: ClassDeclaration | ClassExpression,
      instanceMembers: $ReadOnlyMap<string, InstanceMember>,
      parent: ?ClassStack,
    }>;
    let classStack: ?ClassStack = null;

    return {
      // find and delete duplicated members
      'ClassDeclaration, ClassExpression'(
        node: ClassDeclaration | ClassExpression,
      ) {
        const staticMembers = new Map<string, Array<$ReadOnly<ClassMember>>>();
        const instanceMembers = new Map<
          string,
          Array<$ReadOnly<ClassMember>>,
        >();

        for (const member of node.body.body) {
          const membersMap = member.static ? staticMembers : instanceMembers;
          const name = getClassMemberName(member);
          if (name == null) {
            // unsupported computed member
            continue;
          }

          const existingMember = membersMap.get(name);
          if (existingMember == null) {
            membersMap.set(name, [member]);
          } else {
            existingMember.push(member);
          }
        }

        const toDelete = new Set(
          findDuplicates(staticMembers).concat(findDuplicates(instanceMembers)),
        );

        if (toDelete.size === 0) {
          return;
        }

        // mark members for deletion
        for (const memberToDelete of toDelete) {
          if (
            memberToDelete.type === 'PropertyDefinition' &&
            memberToDelete.value?.type === 'CallExpression'
          ) {
            console.log(
              context.buildCodeFrame(
                memberToDelete.value,
                'Deleted a property with a CallExpression value. You should double check this was safe.',
              ),
            );
          }
          context.removeNode(memberToDelete);
        }

        // we need the final list of instance members so we can
        // fix up rebinding later
        const newInstanceMembers = new Map<string, InstanceMember>();
        for (const member of node.body.body) {
          if (toDelete.has(member)) {
            continue;
          }

          if (member.static) {
            continue;
          }

          const name = getClassMemberName(member);
          if (name == null) {
            continue;
          }

          const kind = getKind(member);
          newInstanceMembers.set(name, {
            kind,
            member,
          });
        }

        // push the stack
        classStack = {
          current: node,
          instanceMembers: newInstanceMembers,
          parent: classStack,
        };
      },

      // remove unnecessary .bind in constructor
      'MethodDefinition[kind = "constructor"] ExpressionStatement > AssignmentExpression[operator = "="] > MemberExpression[object.type = "ThisExpression"].left'(
        node: MemberExpression,
      ) {
        if (!classStack) {
          return;
        }

        const instanceMembers = classStack.instanceMembers;
        if (node.property.type !== 'Identifier') {
          return;
        }
        const subjectName = node.property.name;
        const member = instanceMembers.get(subjectName);
        // the subject member must be a method
        if (member?.kind !== 'Method') {
          return;
        }

        const assignmentExpr = node.parent;
        const exprStatement = assignmentExpr.parent;
        if (
          assignmentExpr.type !== 'AssignmentExpression' ||
          exprStatement.type !== 'ExpressionStatement'
        ) {
          throw new Error('this cannot happen');
        }

        const right = assignmentExpr.right;
        // we're looking for `this.thing = this.thing.bind(this)`
        if (
          right.type !== 'CallExpression' ||
          right.arguments.length !== 1 ||
          right.arguments[0].type !== 'ThisExpression' ||
          right.callee.type !== 'MemberExpression' ||
          right.callee.computed ||
          right.callee.property.type !== 'Identifier' ||
          right.callee.property.name !== 'bind' ||
          right.callee.object.type !== 'MemberExpression' ||
          right.callee.object.computed ||
          right.callee.object.object.type !== 'ThisExpression' ||
          right.callee.object.property.type !== 'Identifier' ||
          right.callee.object.property.name !== subjectName
        ) {
          console.log(
            context.buildCodeFrame(
              right,
              `${subjectName} was reassigned, but not to a bind. You can probably manually correct this.`,
            ),
          );
          return;
        }

        // remove the bind assignment and...
        context.removeStatement(exprStatement);

        const method = member.member;
        if (method.type !== 'MethodDefinition') {
          throw new Error('this cannot happen');
        }

        // convert the method to an arrow function
        replaceMethodWithArrowProp(context, method, subjectName);
      },

      // pop the stack
      'ClassDeclaration, ClassExpression:exit'(
        node: ClassDeclaration | ClassExpression,
      ) {
        if (classStack && classStack?.current === node) {
          classStack = classStack?.parent;
        }
      },
    };
  },
}): Codemod);
