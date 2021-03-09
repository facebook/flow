/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

const syntaxFlow = require('@babel/plugin-syntax-flow').default;

function memberInit(t, bodyType, member) {
  if (bodyType === 'EnumSymbolBody') {
    return t.callExpression(t.identifier('Symbol'), [
      t.stringLiteral(member.id.name),
    ]);
  }
  return member.init;
}

module.exports = function transformEnums(babel) {
  const t = babel.types;

  return {
    name: 'transform-flow-enums',
    inherits: (api, options, dirname) =>
      syntaxFlow(api, {...options, enums: true}, dirname),
    visitor: {
      EnumDeclaration(path, state) {
        const opts = state.opts;
        const enumModule =
          opts.getRuntime != null
            ? opts.getRuntime(t)
            : t.callExpression(t.identifier('require'), [
                t.stringLiteral('flow-enums-runtime'),
              ]);

        const body = path.node.body;
        const members = body.members;
        const mirrored =
          body.type === 'EnumStringBody' &&
          (!members.length || members[0].type === 'EnumDefaultedMember');
        const enumExpression = mirrored
          ? t.callExpression(
              t.memberExpression(enumModule, t.identifier('Mirrored')),
              [
                t.arrayExpression(
                  members.map(member => t.stringLiteral(member.id.name)),
                ),
              ],
            )
          : t.callExpression(enumModule, [
              t.objectExpression(
                members.map(member => {
                  return t.objectProperty(
                    member.id,
                    memberInit(t, body.type, member),
                  );
                }),
              ),
            ]);

        const enumDeclaration = t.variableDeclaration('const', [
          t.variableDeclarator(path.node.id, enumExpression),
        ]);

        // Default exports do not support variable declaration statements as
        // children, instead we need to replace the statement and append a
        // default export of the identifier. e.g.
        //   export default enum A {} -> const A = ...; export default A;
        if (t.isExportDefaultDeclaration(path.parentPath)) {
          path.parentPath.replaceWithMultiple([
            enumDeclaration,
            t.exportDefaultDeclaration(t.identifier(path.node.id.name)),
          ]);
          return;
        }

        path.replaceWith(enumDeclaration);
      },
    },
  };
};
