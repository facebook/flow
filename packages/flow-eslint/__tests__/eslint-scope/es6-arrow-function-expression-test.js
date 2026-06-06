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
 Copyright (C) 2014 Yusuke Suzuki <utatane.tea@gmail.com>

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

describe('ES6 arrow function expression', () => {
  it('materialize scope for arrow function expression', () => {
    const {scopeManager} = parseForESLint(`
            var arrow = () => {
                let i = 0;
                var j = 20;
                console.log(i);
            }
        `);

    expect(scopeManager.scopes).toHaveLength(2);

    let scope = scopeManager.scopes[0];

    expect(scope.type).toEqual('global');
    expect(scope.block.type).toEqual('Program');
    expect(scope.isStrict).toBe(false);
    expect(scope.variables).toHaveLength(1);

    scope = scopeManager.scopes[1];
    expect(scope.type).toEqual('function');
    expect(scope.block.type).toEqual('ArrowFunctionExpression');
    expect(scope.isStrict).toBe(false);
    expect(scope.variables).toHaveLength(2);

    // There's no "arguments"
    expect(scope.variables[0].name).toEqual('i');
    expect(scope.variables[1].name).toEqual('j');
  });

  it('generate bindings for parameters', () => {
    const {scopeManager} = parseForESLint('var arrow = (a, b, c, d) => {}');

    expect(scopeManager.scopes).toHaveLength(2);

    let scope = scopeManager.scopes[0];

    expect(scope.type).toEqual('global');
    expect(scope.block.type).toEqual('Program');
    expect(scope.isStrict).toBe(false);
    expect(scope.variables).toHaveLength(1);

    scope = scopeManager.scopes[1];
    expect(scope.type).toEqual('function');
    expect(scope.block.type).toEqual('ArrowFunctionExpression');
    expect(scope.isStrict).toBe(false);
    expect(scope.variables).toHaveLength(4);

    // There's no "arguments"
    expect(scope.variables[0].name).toEqual('a');
    expect(scope.variables[1].name).toEqual('b');
    expect(scope.variables[2].name).toEqual('c');
    expect(scope.variables[3].name).toEqual('d');
  });

  it('inherits upper scope strictness', () => {
    const {scopeManager} = parseForESLint(`
            "use strict";
            var arrow = () => {};
        `);

    expect(scopeManager.scopes).toHaveLength(2);

    let scope = scopeManager.scopes[0];

    expect(scope.type).toEqual('global');
    expect(scope.block.type).toEqual('Program');
    expect(scope.isStrict).toBe(true);
    expect(scope.variables).toHaveLength(1);

    scope = scopeManager.scopes[1];

    expect(scope.type).toEqual('function');
    expect(scope.block.type).toEqual('ArrowFunctionExpression');
    expect(scope.isStrict).toBe(true);
    expect(scope.variables).toHaveLength(0);
  });

  it('is strict when a strictness directive is used', () => {
    const {scopeManager} = parseForESLint(`
            var arrow = () => {
                "use strict";
            };
        `);

    expect(scopeManager.scopes).toHaveLength(2);

    let scope = scopeManager.scopes[0];

    expect(scope.type).toEqual('global');
    expect(scope.block.type).toEqual('Program');
    expect(scope.isStrict).toBe(false);
    expect(scope.variables).toHaveLength(1);

    scope = scopeManager.scopes[1];

    expect(scope.type).toEqual('function');
    expect(scope.block.type).toEqual('ArrowFunctionExpression');
    expect(scope.isStrict).toBe(true);
    expect(scope.variables).toHaveLength(0);
  });

  it('works with no body', () => {
    const {scopeManager} = parseForESLint('var arrow = a => a;');

    expect(scopeManager.scopes).toHaveLength(2);

    const scope = scopeManager.scopes[1];

    expect(scope.type).toEqual('function');
    expect(scope.block.type).toEqual('ArrowFunctionExpression');
    expect(scope.isStrict).toBe(false);
    expect(scope.variables).toHaveLength(1);
  });
});
