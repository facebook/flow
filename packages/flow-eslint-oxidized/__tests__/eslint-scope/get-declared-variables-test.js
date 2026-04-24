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

import type {Program, ESNode} from 'flow-estree-oxidized';

// $FlowExpectedError[untyped-import]
import {visit} from 'esrecurse';
import {parse} from '../../src';
import {analyze} from '../../src/scope-manager';

describe('ScopeManager.prototype.getDeclaredVariables', () => {
  function verify(
    ast: Program,
    type: string,
    expectedNamesList: Array<Array<string>>,
  ) {
    const scopeManager = analyze(ast, {
      sourceType: 'module',
    });

    visit(ast, {
      // $FlowExpectedError[invalid-computed-prop]
      [type](this: $FlowFixMe, node: ESNode) {
        const expected = expectedNamesList.shift();
        const actual = scopeManager.getDeclaredVariables(node);

        // $FlowFixMe[incompatible-use]
        expect(actual).toHaveLength(expected.length);
        if (actual.length > 0) {
          const end = actual.length - 1;

          for (let i = 0; i <= end; i++) {
            // $FlowFixMe[incompatible-use]
            expect(actual[i].name).toEqual(expected[i]);
          }
        }

        // $FlowExpectedError[object-this-reference]
        this.visitChildren(node);
      },
    });

    expect(expectedNamesList).toHaveLength(0);
  }

  it('should get variables that declared on `VariableDeclaration`', () => {
    const ast = parse(`
            var {a, x: [b], y: {c = 0}} = foo;
            let {d, x: [e], y: {f = 0}} = foo;
            const {g, x: [h], y: {i = 0}} = foo, {j, k = function() { let l; }} = bar;
        `);

    verify(ast, 'VariableDeclaration', [
      ['a', 'b', 'c'],
      ['d', 'e', 'f'],
      ['g', 'h', 'i', 'j', 'k'],
      ['l'],
    ]);
  });

  it('should get variables that declared on `VariableDeclaration` in for-in/of', () => {
    const ast = parse(`
            for (var {a, x: [b], y: {c = 0}} in foo) {
                let g;
            }
            for (let {d, x: [e], y: {f = 0}} of foo) {
                let h;
            }
        `);

    verify(ast, 'VariableDeclaration', [
      ['a', 'b', 'c'],
      ['g'],
      ['d', 'e', 'f'],
      ['h'],
    ]);
  });

  it('should get variables that declared on `VariableDeclarator`', () => {
    const ast = parse(`
            var {a, x: [b], y: {c = 0}} = foo;
            let {d, x: [e], y: {f = 0}} = foo;
            const {g, x: [h], y: {i = 0}} = foo, {j, k = function() { let l; }} = bar;
        `);

    verify(ast, 'VariableDeclarator', [
      ['a', 'b', 'c'],
      ['d', 'e', 'f'],
      ['g', 'h', 'i'],
      ['j', 'k'],
      ['l'],
    ]);
  });

  it('should get variables that declared on `FunctionDeclaration`', () => {
    const ast = parse(`
            function foo({a, x: [b], y: {c = 0}}, [d, e]) {
                let z;
            }
            // function bar({f, x: [g], y: {h = 0}}, [i, j = function(q) { let w; }]) {
            //     let z;
            // }
        `);

    verify(ast, 'FunctionDeclaration', [
      ['foo', 'a', 'b', 'c', 'd', 'e'],
      // ['bar', 'f', 'g', 'h', 'i', 'j'],
    ]);
  });

  it('should get variables that declared on `FunctionExpression`', () => {
    const ast = parse(`
            (function foo({a, x: [b], y: {c = 0}}, [d, e]) {
                let z;
            });
            (function bar({f, x: [g], y: {h = 0}}, [i, j = function(q) { let w; }]) {
                let z;
            });
        `);

    verify(ast, 'FunctionExpression', [
      ['foo', 'a', 'b', 'c', 'd', 'e'],
      ['bar', 'f', 'g', 'h', 'i', 'j'],
      ['q'],
    ]);
  });

  it('should get variables that declared on `ArrowFunctionExpression`', () => {
    const ast = parse(`
            (({a, x: [b], y: {c = 0}}, [d, e]) => {
                let z;
            });
            (({f, x: [g], y: {h = 0}}, [i, j]) => {
                let z;
            });
        `);

    verify(ast, 'ArrowFunctionExpression', [
      ['a', 'b', 'c', 'd', 'e'],
      ['f', 'g', 'h', 'i', 'j'],
    ]);
  });

  it('should get variables that declared on `ClassDeclaration`', () => {
    const ast = parse(`
            class A { foo(x) { let y; } }
            class B { foo(x) { let y; } }
        `);

    verify(ast, 'ClassDeclaration', [
      ['A', 'A'], // outer scope"s and inner scope"s.
      ['B', 'B'],
    ]);
  });

  it('should get variables that declared on `ClassExpression`', () => {
    const ast = parse(`
            (class A { foo(x) { let y; } });
            (class B { foo(x) { let y; } });
        `);

    verify(ast, 'ClassExpression', [['A'], ['B']]);
  });

  it('should get variables that declared on `CatchClause`', () => {
    const ast = parse(`
            try {} catch ({a, b}) {
                let x;
                try {} catch ({c, d}) {
                    let y;
                }
            }
        `);

    verify(ast, 'CatchClause', [
      ['a', 'b'],
      ['c', 'd'],
    ]);
  });

  it('should get variables that declared on `ImportDeclaration`', () => {
    const ast = parse(`
            import "aaa";
            import * as a from "bbb";
            import b, {c, x as d} from "ccc";`);

    verify(ast, 'ImportDeclaration', [[], ['a'], ['b', 'c', 'd']]);
  });

  it('should get variables that declared on `ImportSpecifier`', () => {
    const ast = parse(`
            import "aaa";
            import * as a from "bbb";
            import b, {c, x as d} from "ccc";`);

    verify(ast, 'ImportSpecifier', [['c'], ['d']]);
  });

  it('should get variables that declared on `ImportDefaultSpecifier`', () => {
    const ast = parse(`
            import "aaa";
            import * as a from "bbb";
            import b, {c, x as d} from "ccc";`);

    verify(ast, 'ImportDefaultSpecifier', [['b']]);
  });

  it('should get variables that declared on `ImportNamespaceSpecifier`', () => {
    const ast = parse(`
            import "aaa";
            import * as a from "bbb";
            import b, {c, x as d} from "ccc";`);

    verify(ast, 'ImportNamespaceSpecifier', [['a']]);
  });

  it("should not get duplicate even if it's declared twice", () => {
    const ast = parse('var a = 0, a = 1;');

    verify(ast, 'VariableDeclaration', [['a']]);
  });
});
