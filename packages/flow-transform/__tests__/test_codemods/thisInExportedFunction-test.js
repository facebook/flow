/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import {t, transform} from './test-utils';

function codemod(code: string) {
  return transform(code, context => ({
    // find member expressions within exported functions that don't have another function boundary between them
    ':matches(ExportNamedDeclaration, ExportDefaultDeclaration) > FunctionDeclaration MemberExpression'(
      node,
    ) {
      if (
        node.type !== 'MemberExpression' ||
        node.property.type !== 'Identifier' ||
        node.object.type !== 'ThisExpression'
      ) {
        return;
      }
      const targetName = node.property.name;
      const targetVar = context.getBinding(targetName);
      const topScope = (() => {
        let currentScope = context.getScope();
        while (currentScope != null) {
          if (currentScope.type === 'module') {
            return currentScope;
          }
          if (currentScope.type === 'global') {
            return currentScope;
          }
          currentScope = currentScope.upper;
        }

        // this won't ever actually happen because it's just how scopes work
        throw new Error(
          'Impossible state during traversal - unable to find a parent module or global scope.',
        );
      })();
      if (!targetVar || targetVar.defs.length === 0) {
        // we don't know of a variable for the identifier - so it's not a globally exported function
        return;
      }

      if (targetVar.scope !== topScope) {
        // there is a local variable with the same name
        return;
      }

      const isTargetDirectlyExported = (() => {
        let isExported = false;
        for (const def of targetVar.defs) {
          switch (def.type) {
            case 'Enum':
              isExported =
                isExported ||
                def.node.parent.type === 'ExportDefaultDeclaration' ||
                def.node.parent.type === 'ExportNamedDeclaration';
              break;
            case 'ClassName':
            case 'FunctionName':
            case 'Variable':
              if (
                def.node.type === 'DeclareFunction' ||
                def.node.type === 'DeclareClass' ||
                def.node.type === 'DeclareVariable'
              ) {
                isExported =
                  isExported ||
                  def.node.parent.type === 'DeclareExportDeclaration';
              } else {
                isExported =
                  isExported ||
                  def.node.parent.type === 'ExportDefaultDeclaration' ||
                  def.node.parent.type === 'ExportNamedDeclaration';
              }
              break;

            case 'CatchClause':
            case 'ImplicitGlobalVariable':
            case 'ImportBinding':
            case 'Parameter':
            case 'Type':
            case 'TypeParameter':
              return false;
          }
        }
        return isExported;
      })();
      if (!isTargetDirectlyExported) {
        // target isn't exported - so it's unlikely the `this.` referenced it
        // target could be indirectly exported.. but that's unlikely
        return;
      }

      const functionParent = (() => {
        let functionDeclCount = 0;
        let functionDecl;
        let current = node.parent;
        while (current != null) {
          if (current.type === 'FunctionDeclaration') {
            functionDecl = current;
            functionDeclCount += 1;
          }

          if (
            current.type === 'FunctionExpression' ||
            current.type === 'ClassDeclaration' ||
            current.type === 'ClassExpression'
          ) {
            // these all create a new "this" scope, so if we encounter one in the parent chain
            // then it means that the `this` of our member expr doesn't reference the "global" this.
            return null;
          }

          current = current.parent;
        }

        if (functionDeclCount > 1) {
          // same reasons as the other node types
          // multiple function decls is not right
          return null;
        }
        return functionDecl;
      })();
      if (functionParent == null) {
        // there was an invalid 'this' boundary
        return;
      }
      if (
        functionParent.params.length > 0 &&
        functionParent.params[0].type === 'Identifier' &&
        functionParent.params[0].name === 'this'
      ) {
        // the function has a this param, so it's "safe" to reference `this`
        return;
      }

      // we now know that the `foo` referenced by `this.foo` is a top-level variable
      // and that `foo` is exported
      // so we can just directly reference it
      context.replaceNode(node, t.Identifier({name: targetName}));
    },
  }));
}

describe('React to react', () => {
  it('should transform valid cases correctly', async () => {
    const result = await codemod(`\
export function foo() {}

export function bar() {
  this.foo();
}
export function baz() {
  if (true) {
    while (false) {
      return () => this.foo();
    }
  }
}
`);

    expect(result).toBe(`\
export function foo() {}

export function bar() {
  foo();
}
export function baz() {
  if (true) {
    while (false) {
      return () => foo();
    }
  }
}
`);
  });

  it('should ignore invalid cases correctly', async () => {
    const result = await codemod(`\
function foo() {}
type Type = {};

export function bar() {
  this.foo();
}
export function baz() {
  function inner() {
    this.bar();
  }
}
export function bam() {
  this.unknown();
}
export function bang() {
  this.Type();
}
`);

    expect(result).toBe(`\
function foo() {}
type Type = {};

export function bar() {
  this.foo();
}
export function baz() {
  function inner() {
    this.bar();
  }
}
export function bam() {
  this.unknown();
}
export function bang() {
  this.Type();
}
`);
  });
});
