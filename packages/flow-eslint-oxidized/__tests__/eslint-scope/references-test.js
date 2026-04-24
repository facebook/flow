/**
 * Portions Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
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

const {parseForESLint} = require('./eslint-scope-test-utils');

describe('References:', () => {
  describe('When there is a `let` declaration on global,', () => {
    it('the reference on global should be resolved.', () => {
      const {scopeManager} = parseForESLint('let a = 0;');

      expect(scopeManager.scopes).toHaveLength(1);

      const scope = scopeManager.scopes[0];

      expect(scope.variables).toHaveLength(1);
      expect(scope.references).toHaveLength(1);

      const reference = scope.references[0];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).toEqual(scope.variables[0]);
      expect(reference.writeExpr).toBeDefined();
      expect(reference.isWrite()).toBe(true);
      expect(reference.isRead()).toBe(false);
    });

    it('the reference in functions should be resolved.', () => {
      const {scopeManager} = parseForESLint(`
                let a = 0;
                function foo() {
                    let b = a;
                }
            `);

      expect(scopeManager.scopes).toHaveLength(2); // [global, foo]

      const scope = scopeManager.scopes[1];

      expect(scope.variables).toHaveLength(2); // [arguments, b]
      expect(scope.references).toHaveLength(2); // [b, a]

      const reference = scope.references[1];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).toEqual(scopeManager.scopes[0].variables[0]);
      expect(reference.writeExpr).toBeUndefined();
      expect(reference.isWrite()).toBe(false);
      expect(reference.isRead()).toBe(true);
    });

    it('the reference in default parameters should be resolved.', () => {
      const {scopeManager} = parseForESLint(`
                let a = 0;
                function foo(b = a) {
                }
            `);

      expect(scopeManager.scopes).toHaveLength(2); // [global, foo]

      const scope = scopeManager.scopes[1];

      expect(scope.variables).toHaveLength(2); // [arguments, b]
      expect(scope.references).toHaveLength(2); // [b, a]

      const reference = scope.references[1];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).toEqual(scopeManager.scopes[0].variables[0]);
      expect(reference.writeExpr).toBeUndefined();
      expect(reference.isWrite()).toBe(false);
      expect(reference.isRead()).toBe(true);
    });
  });

  describe('When there is a `const` declaration on global,', () => {
    it('the reference on global should be resolved.', () => {
      const {scopeManager} = parseForESLint('const a = 0;');

      expect(scopeManager.scopes).toHaveLength(1);

      const scope = scopeManager.scopes[0];

      expect(scope.variables).toHaveLength(1);
      expect(scope.references).toHaveLength(1);

      const reference = scope.references[0];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).toEqual(scope.variables[0]);
      expect(reference.writeExpr).toBeDefined();
      expect(reference.isWrite()).toBe(true);
      expect(reference.isRead()).toBe(false);
    });

    it('the reference in functions should be resolved.', () => {
      const {scopeManager} = parseForESLint(`
                const a = 0;
                function foo() {
                    const b = a;
                }
            `);

      expect(scopeManager.scopes).toHaveLength(2); // [global, foo]

      const scope = scopeManager.scopes[1];

      expect(scope.variables).toHaveLength(2); // [arguments, b]
      expect(scope.references).toHaveLength(2); // [b, a]

      const reference = scope.references[1];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).toEqual(scopeManager.scopes[0].variables[0]);
      expect(reference.writeExpr).toBeUndefined();
      expect(reference.isWrite()).toBe(false);
      expect(reference.isRead()).toBe(true);
    });
  });

  describe('When there is a `var` declaration on global,', () => {
    it('the reference on global should NOT be resolved.', () => {
      const {scopeManager} = parseForESLint('var a = 0;');

      expect(scopeManager.scopes).toHaveLength(1);

      const scope = scopeManager.scopes[0];

      expect(scope.variables).toHaveLength(1);
      expect(scope.references).toHaveLength(1);

      const reference = scope.references[0];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).toBeNull();
      expect(reference.writeExpr).toBeDefined();
      expect(reference.isWrite()).toBe(true);
      expect(reference.isRead()).toBe(false);
    });

    it('the reference in functions should NOT be resolved.', () => {
      const {scopeManager} = parseForESLint(`
                var a = 0;
                function foo() {
                    var b = a;
                }
            `);

      expect(scopeManager.scopes).toHaveLength(2); // [global, foo]

      const scope = scopeManager.scopes[1];

      expect(scope.variables).toHaveLength(2); // [arguments, b]
      expect(scope.references).toHaveLength(2); // [b, a]

      const reference = scope.references[1];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).toBeNull();
      expect(reference.writeExpr).toBeUndefined();
      expect(reference.isWrite()).toBe(false);
      expect(reference.isRead()).toBe(true);
    });
  });

  describe('When there is a `function` declaration on global,', () => {
    it('the reference on global should be resolved.', () => {
      const {scopeManager} = parseForESLint(`
                function a() {}
                a();
            `);

      expect(scopeManager.scopes).toHaveLength(2); // [global, a]

      const scope = scopeManager.scopes[0];

      expect(scope.variables).toHaveLength(1);
      expect(scope.references).toHaveLength(1);

      const reference = scope.references[0];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).not.toBeNull();
      expect(reference.writeExpr).toBeUndefined();
      expect(reference.isWrite()).toBe(false);
      expect(reference.isRead()).toBe(true);
    });

    it('the reference in functions should be resolved.', () => {
      const {scopeManager} = parseForESLint(`
                function a() {}
                function foo() {
                    let b = a();
                }
            `);

      expect(scopeManager.scopes).toHaveLength(3); // [global, a, foo]

      const scope = scopeManager.scopes[2];

      expect(scope.variables).toHaveLength(2); // [arguments, b]
      expect(scope.references).toHaveLength(2); // [b, a]

      const reference = scope.references[1];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).not.toBeNull();
      expect(reference.writeExpr).toBeUndefined();
      expect(reference.isWrite()).toBe(false);
      expect(reference.isRead()).toBe(true);
    });
  });

  describe('When there is a `class` declaration on global,', () => {
    it('the reference on global should be resolved.', () => {
      const {scopeManager} = parseForESLint(`
                class A {}
                let b = new A();
            `);

      expect(scopeManager.scopes).toHaveLength(2); // [global, A]

      const scope = scopeManager.scopes[0];

      expect(scope.variables).toHaveLength(2); // [A, b]
      expect(scope.references).toHaveLength(2); // [b, A]

      const reference = scope.references[1];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('A');
      expect(reference.resolved).toEqual(scope.variables[0]);
      expect(reference.writeExpr).toBeUndefined();
      expect(reference.isWrite()).toBe(false);
      expect(reference.isRead()).toBe(true);
    });

    it('the reference in functions should be resolved.', () => {
      const {scopeManager} = parseForESLint(`
                class A {}
                function foo() {
                    let b = new A();
                }
            `);

      expect(scopeManager.scopes).toHaveLength(3); // [global, A, foo]

      const scope = scopeManager.scopes[2];

      expect(scope.variables).toHaveLength(2); // [arguments, b]
      expect(scope.references).toHaveLength(2); // [b, A]

      const reference = scope.references[1];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('A');
      expect(reference.resolved).toEqual(scopeManager.scopes[0].variables[0]);
      expect(reference.writeExpr).toBeUndefined();
      expect(reference.isWrite()).toBe(false);
      expect(reference.isRead()).toBe(true);
    });
  });

  describe('When there is a `let` declaration in functions,', () => {
    it('the reference on the function should be resolved.', () => {
      const {scopeManager} = parseForESLint(`
                function foo() {
                    let a = 0;
                }
            `);

      expect(scopeManager.scopes).toHaveLength(2); // [global, foo]

      const scope = scopeManager.scopes[1];

      expect(scope.variables).toHaveLength(2); // [arguments, a]
      expect(scope.references).toHaveLength(1);

      const reference = scope.references[0];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).toEqual(scope.variables[1]);
      expect(reference.writeExpr).toBeDefined();
      expect(reference.isWrite()).toBe(true);
      expect(reference.isRead()).toBe(false);
    });

    it('the reference in nested functions should be resolved.', () => {
      const {scopeManager} = parseForESLint(`
                function foo() {
                    let a = 0;
                    function bar() {
                        let b = a;
                    }
                }
            `);

      expect(scopeManager.scopes).toHaveLength(3); // [global, foo, bar]

      const scope = scopeManager.scopes[2];

      expect(scope.variables).toHaveLength(2); // [arguments, b]
      expect(scope.references).toHaveLength(2); // [b, a]

      const reference = scope.references[1];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).toEqual(scopeManager.scopes[1].variables[1]);
      expect(reference.writeExpr).toBeUndefined();
      expect(reference.isWrite()).toBe(false);
      expect(reference.isRead()).toBe(true);
    });
  });

  describe('When there is a `var` declaration in functions,', () => {
    it('the reference on the function should be resolved.', () => {
      const {scopeManager} = parseForESLint(`
                function foo() {
                    var a = 0;
                }
            `);

      expect(scopeManager.scopes).toHaveLength(2); // [global, foo]

      const scope = scopeManager.scopes[1];

      expect(scope.variables).toHaveLength(2); // [arguments, a]
      expect(scope.references).toHaveLength(1);

      const reference = scope.references[0];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).toEqual(scope.variables[1]);
      expect(reference.writeExpr).toBeDefined();
      expect(reference.isWrite()).toBe(true);
      expect(reference.isRead()).toBe(false);
    });

    it('the reference in nested functions should be resolved.', () => {
      const {scopeManager} = parseForESLint(`
                function foo() {
                    var a = 0;
                    function bar() {
                        var b = a;
                    }
                }
            `);

      expect(scopeManager.scopes).toHaveLength(3); // [global, foo, bar]

      const scope = scopeManager.scopes[2];

      expect(scope.variables).toHaveLength(2); // [arguments, b]
      expect(scope.references).toHaveLength(2); // [b, a]

      const reference = scope.references[1];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).toEqual(scopeManager.scopes[1].variables[1]);
      expect(reference.writeExpr).toBeUndefined();
      expect(reference.isWrite()).toBe(false);
      expect(reference.isRead()).toBe(true);
    });
  });

  describe('When there is a `let` declaration with destructuring assignment', () => {
    it('"let [a] = [1];", the reference should be resolved.', () => {
      const {scopeManager} = parseForESLint('let [a] = [1];');

      expect(scopeManager.scopes).toHaveLength(1);

      const scope = scopeManager.scopes[0];

      expect(scope.variables).toHaveLength(1);
      expect(scope.references).toHaveLength(1);

      const reference = scope.references[0];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).toEqual(scope.variables[0]);
      expect(reference.writeExpr).toBeDefined();
      expect(reference.isWrite()).toBe(true);
      expect(reference.isRead()).toBe(false);
    });

    it('"let {a} = {a: 1};", the reference should be resolved.', () => {
      const {scopeManager} = parseForESLint('let {a} = {a: 1};');

      expect(scopeManager.scopes).toHaveLength(1);

      const scope = scopeManager.scopes[0];

      expect(scope.variables).toHaveLength(1);
      expect(scope.references).toHaveLength(1);

      const reference = scope.references[0];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).toEqual(scope.variables[0]);
      expect(reference.writeExpr).toBeDefined();
      expect(reference.isWrite()).toBe(true);
      expect(reference.isRead()).toBe(false);
    });

    it('"let {a: {a}} = {a: {a: 1}};", the reference should be resolved.', () => {
      const {scopeManager} = parseForESLint('let {a: {a}} = {a: {a: 1}};');

      expect(scopeManager.scopes).toHaveLength(1);

      const scope = scopeManager.scopes[0];

      expect(scope.variables).toHaveLength(1);
      expect(scope.references).toHaveLength(1);

      const reference = scope.references[0];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).toEqual(scope.variables[0]);
      expect(reference.writeExpr).toBeDefined();
      expect(reference.isWrite()).toBe(true);
      expect(reference.isRead()).toBe(false);
    });
  });

  describe('Reference.init should be a boolean value of whether it is one to initialize or not.', () => {
    const trueCodes = [
      'var a = 0;',
      'let a = 0;',
      'const a = 0;',
      'var [a] = [];',
      'let [a] = [];',
      'const [a] = [];',
      'var [a = 1] = [];',
      'let [a = 1] = [];',
      'const [a = 1] = [];',
      'var {a} = {};',
      'let {a} = {};',
      'const {a} = {};',
      'var {b: a} = {};',
      'let {b: a} = {};',
      'const {b: a} = {};',
      'var {b: a = 0} = {};',
      'let {b: a = 0} = {};',
      'const {b: a = 0} = {};',
      'for (var a in []);',
      'for (let a in []);',
      'for (var [a] in []);',
      'for (let [a] in []);',
      'for (var [a = 0] in []);',
      'for (let [a = 0] in []);',
      'for (var {a} in []);',
      'for (let {a} in []);',
      'for (var {a = 0} in []);',
      'for (let {a = 0} in []);',
      'new function(a = 0) {}',
      'new function([a = 0] = []) {}',
      'new function({b: a = 0} = {}) {}',
    ];

    trueCodes.forEach(code =>
      it(`"${code}", all references should be true.`, () => {
        const {scopeManager} = parseForESLint(code);

        expect(scopeManager.scopes.length).toBeGreaterThanOrEqual(1);

        const scope = scopeManager.scopes[scopeManager.scopes.length - 1];

        expect(scope.variables.length).toBeGreaterThanOrEqual(1);
        expect(scope.references.length).toBeGreaterThanOrEqual(1);

        scope.references.forEach(reference => {
          expect(reference.identifier.name).toEqual('a');
          expect(reference.isWrite()).toBe(true);
          expect(reference.init).toBe(true);
        });
      }),
    );

    let falseCodes = [
      'let a; a = 0;',
      'let a; [a] = [];',
      'let a; [a = 1] = [];',
      'let a; ({a} = {});',
      'let a; ({b: a} = {});',
      'let a; ({b: a = 0} = {});',
      'let a; for (a in []);',
      'let a; for ([a] in []);',
      'let a; for ([a = 0] in []);',
      'let a; for ({a} in []);',
      'let a; for ({a = 0} in []);',
    ];

    falseCodes.forEach(code =>
      it(`"${code}", all references should be false.`, () => {
        const {scopeManager} = parseForESLint(code);

        expect(scopeManager.scopes.length).toBeGreaterThanOrEqual(1);

        const scope = scopeManager.scopes[scopeManager.scopes.length - 1];

        expect(scope.variables).toHaveLength(1);
        expect(scope.references.length).toBeGreaterThanOrEqual(1);

        scope.references.forEach(reference => {
          expect(reference.identifier.name).toEqual('a');
          expect(reference.isWrite()).toBe(true);
          expect(reference.init).toBe(false);
        });
      }),
    );

    falseCodes = [
      'let a; let b = a;',
      'let a; let [b] = a;',
      'let a; let [b = a] = [];',
      'let a; for (var b in a);',
      'let a; for (var [b = a] in []);',
      'let a; for (let b in a);',
      'let a; for (let [b = a] in []);',
      'let a,b; b = a;',
      'let a,b; [b] = a;',
      'let a,b; [b = a] = [];',
      'let a,b; for (b in a);',
      'let a,b; for ([b = a] in []);',
      'let a; a.foo = 0;',
      'let a,b; b = a.foo;',
    ];
    falseCodes.forEach(code =>
      it(`"${code}", readonly references of "a" should be undefined.`, () => {
        const {scopeManager} = parseForESLint(code);

        expect(scopeManager.scopes.length).toBeGreaterThanOrEqual(1);

        const scope = scopeManager.scopes[0];

        expect(scope.variables.length).toBeGreaterThanOrEqual(1);
        expect(scope.variables[0].name).toEqual('a');

        const references = scope.variables[0].references;

        expect(references.length).toBeGreaterThanOrEqual(1);

        references.forEach(reference => {
          expect(reference.isRead()).toBe(true);
          expect(reference.init).toBeUndefined();
        });
      }),
    );
  });
});
