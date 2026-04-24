/**
 * Portions Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

/*
 Copyright (C) 2015 Toru Nagashima

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
'use strict';

import {parseForESLint} from './eslint-scope-test-utils';

describe('ES6 default parameters:', () => {
  describe('a default parameter creates a writable reference for its initialization:', () => {
    const patterns = {
      FunctionDeclaration: 'function foo(a, b = 0) {}',
      FunctionExpression: 'let foo = function(a, b = 0) {};',
      ArrowExpression: 'let foo = (a, b = 0) => {};',
    };

    for (const name in patterns) {
      // $FlowFixMe[invalid-computed-prop]
      const code = patterns[name];

      it(name, () => {
        const numVars = name === 'ArrowExpression' ? 2 : 3;
        const {scopeManager} = parseForESLint(code);

        expect(scopeManager.scopes).toHaveLength(2); // [global, foo]

        const scope = scopeManager.scopes[1];

        expect(scope.variables).toHaveLength(numVars); // [arguments?, a, b]
        expect(scope.references).toHaveLength(1);

        const reference = scope.references[0];

        expect(reference.from).toEqual(scope);
        expect(reference.identifier.name).toEqual('b');
        expect(reference.resolved).toEqual(scope.variables[numVars - 1]);
        expect(reference.writeExpr).toBeDefined();
        expect(reference.isWrite()).toBe(true);
        expect(reference.isRead()).toBe(false);
      });
    }
  });

  describe('a default parameter creates a readable reference for references in right:', () => {
    const patterns = {
      FunctionDeclaration: `
                let a;
                function foo(b = a) {}
            `,
      FunctionExpression: `
                let a;
                let foo = function(b = a) {}
            `,
      ArrowExpression: `
                let a;
                let foo = (b = a) => {};
            `,
    };

    for (const name in patterns) {
      // $FlowFixMe[invalid-computed-prop]
      const code = patterns[name];

      it(name, () => {
        const numVars = name === 'ArrowExpression' ? 1 : 2;
        const {scopeManager} = parseForESLint(code);

        expect(scopeManager.scopes).toHaveLength(2); // [global, foo]

        const scope = scopeManager.scopes[1];

        expect(scope.variables).toHaveLength(numVars); // [arguments?, b]
        expect(scope.references).toHaveLength(2); // [b, a]

        const reference = scope.references[1];

        expect(reference.from).toEqual(scope);
        expect(reference.identifier.name).toEqual('a');
        expect(reference.resolved).toEqual(scopeManager.scopes[0].variables[0]);
        expect(reference.writeExpr).toBeUndefined();
        expect(reference.isWrite()).toBe(false);
        expect(reference.isRead()).toBe(true);
      });
    }
  });

  describe('a default parameter creates a readable reference for references in right (for const):', () => {
    const patterns = {
      FunctionDeclaration: `
                const a = 0;
                function foo(b = a) {}
            `,
      FunctionExpression: `
                const a = 0;
                let foo = function(b = a) {}
            `,
      ArrowExpression: `
                const a = 0;
                let foo = (b = a) => {};
            `,
    };

    for (const name in patterns) {
      // $FlowFixMe[invalid-computed-prop]
      const code = patterns[name];

      it(name, () => {
        const numVars = name === 'ArrowExpression' ? 1 : 2;
        const {scopeManager} = parseForESLint(code);

        expect(scopeManager.scopes).toHaveLength(2); // [global, foo]

        const scope = scopeManager.scopes[1];

        expect(scope.variables).toHaveLength(numVars); // [arguments?, b]
        expect(scope.references).toHaveLength(2); // [b, a]

        const reference = scope.references[1];

        expect(reference.from).toEqual(scope);
        expect(reference.identifier.name).toEqual('a');
        expect(reference.resolved).toEqual(scopeManager.scopes[0].variables[0]);
        expect(reference.writeExpr).toBeUndefined();
        expect(reference.isWrite()).toBe(false);
        expect(reference.isRead()).toBe(true);
      });
    }
  });

  describe('a default parameter creates a readable reference for references in right (partial):', () => {
    const patterns = {
      FunctionDeclaration: `
                let a;
                function foo(b = a.c) {}
            `,
      FunctionExpression: `
                let a;
                let foo = function(b = a.c) {}
            `,
      ArrowExpression: `
                let a;
                let foo = (b = a.c) => {};
            `,
    };

    for (const name in patterns) {
      // $FlowFixMe[invalid-computed-prop]
      const code = patterns[name];

      it(name, () => {
        const numVars = name === 'ArrowExpression' ? 1 : 2;
        const {scopeManager} = parseForESLint(code);

        expect(scopeManager.scopes).toHaveLength(2); // [global, foo]

        const scope = scopeManager.scopes[1];

        expect(scope.variables).toHaveLength(numVars); // [arguments?, b]
        expect(scope.references).toHaveLength(2); // [b, a]

        const reference = scope.references[1];

        expect(reference.from).toEqual(scope);
        expect(reference.identifier.name).toEqual('a');
        expect(reference.resolved).toEqual(scopeManager.scopes[0].variables[0]);
        expect(reference.writeExpr).toBeUndefined();
        expect(reference.isWrite()).toBe(false);
        expect(reference.isRead()).toBe(true);
      });
    }
  });

  describe("a default parameter creates a readable reference for references in right's nested scope:", () => {
    const patterns = {
      FunctionDeclaration: `
                let a;
                function foo(b = function() { return a; }) {}
            `,
      FunctionExpression: `
                let a;
                let foo = function(b = function() { return a; }) {}
            `,
      ArrowExpression: `
                let a;
                let foo = (b = function() { return a; }) => {};
            `,
    };

    for (const name in patterns) {
      // $FlowFixMe[invalid-computed-prop]
      const code = patterns[name];

      it(name, () => {
        const {scopeManager} = parseForESLint(code);

        expect(scopeManager.scopes).toHaveLength(3); // [global, foo, anonymous]

        const scope = scopeManager.scopes[2];

        expect(scope.variables).toHaveLength(1); // [arguments]
        expect(scope.references).toHaveLength(1); // [a]

        const reference = scope.references[0];

        expect(reference.from).toEqual(scope);
        expect(reference.identifier.name).toEqual('a');
        expect(reference.resolved).toEqual(scopeManager.scopes[0].variables[0]);
        expect(reference.writeExpr).toBeUndefined();
        expect(reference.isWrite()).toBe(false);
        expect(reference.isRead()).toBe(true);
      });
    }
  });

  describe("a default parameter creates a readable reference for references in right. It's resolved to outer scope's even if there is the variable in the function body:", () => {
    const patterns = {
      FunctionDeclaration: `
                let a;
                function foo(b = a) { let a; }
            `,
      FunctionExpression: `
                let a;
                let foo = function(b = a) { let a; }
            `,
      ArrowExpression: `
                let a;
                let foo = (b = a) => { let a; };
            `,
    };

    for (const name in patterns) {
      // $FlowFixMe[invalid-computed-prop]
      const code = patterns[name];

      it(name, () => {
        const numVars = name === 'ArrowExpression' ? 2 : 3;
        const {scopeManager} = parseForESLint(code);

        expect(scopeManager.scopes).toHaveLength(2); // [global, foo]

        const scope = scopeManager.scopes[1];

        expect(scope.variables).toHaveLength(numVars); // [arguments?, b, a]
        expect(scope.references).toHaveLength(2); // [b, a]

        const reference = scope.references[1];

        expect(reference.from).toEqual(scope);
        expect(reference.identifier.name).toEqual('a');
        expect(reference.resolved).toEqual(scopeManager.scopes[0].variables[0]);
        expect(reference.writeExpr).toBeUndefined();
        expect(reference.isWrite()).toBe(false);
        expect(reference.isRead()).toBe(true);
      });
    }
  });

  describe("a default parameter creates a readable reference for references in right. It's resolved to the parameter:", () => {
    const patterns = {
      FunctionDeclaration: `
                let a;
                function foo(b = a, a) { }
            `,
      FunctionExpression: `
                let a;
                let foo = function(b = a, a) { }
            `,
      ArrowExpression: `
                let a;
                let foo = (b = a, a) => { };
            `,
    };

    for (const name in patterns) {
      // $FlowFixMe[invalid-computed-prop]
      const code = patterns[name];

      it(name, () => {
        const numVars = name === 'ArrowExpression' ? 2 : 3;
        const {scopeManager} = parseForESLint(code);

        expect(scopeManager.scopes).toHaveLength(2); // [global, foo]

        const scope = scopeManager.scopes[1];

        expect(scope.variables).toHaveLength(numVars); // [arguments?, b, a]
        expect(scope.references).toHaveLength(2); // [b, a]

        const reference = scope.references[1];

        expect(reference.from).toEqual(scope);
        expect(reference.identifier.name).toEqual('a');
        expect(reference.resolved).toEqual(
          scope.variables[scope.variables.length - 1],
        );
        expect(reference.writeExpr).toBeUndefined();
        expect(reference.isWrite()).toBe(false);
        expect(reference.isRead()).toBe(true);
      });
    }
  });

  describe("a default parameter creates a readable reference for references in right (nested scope). It's resolved to outer scope's even if there is the variable in the function body:", () => {
    const patterns = {
      FunctionDeclaration: `
                let a;
                function foo(b = function(){ a }) { let a; }
            `,
      FunctionExpression: `
                let a;
                let foo = function(b = function(){ a }) { let a; }
            `,
      ArrowExpression: `
                let a;
                let foo = (b = function(){ a }) => { let a; };
            `,
    };

    for (const name in patterns) {
      // $FlowFixMe[invalid-computed-prop]
      const code = patterns[name];

      it(name, () => {
        const {scopeManager} = parseForESLint(code);

        expect(scopeManager.scopes).toHaveLength(3); // [global, foo, anonymous function]

        const scope = scopeManager.scopes[2];

        expect(scope.references).toHaveLength(1); // [a]

        const reference = scope.references[0];

        expect(reference.from).toEqual(scope);
        expect(reference.identifier.name).toEqual('a');
        expect(reference.resolved).toEqual(scopeManager.scopes[0].variables[0]);
        expect(reference.writeExpr).toBeUndefined();
        expect(reference.isWrite()).toBe(false);
        expect(reference.isRead()).toBe(true);
      });
    }
  });
});
