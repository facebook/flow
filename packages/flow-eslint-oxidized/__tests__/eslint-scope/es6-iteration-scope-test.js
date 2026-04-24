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

describe('ES6 iteration scope', () => {
  it('let materialize iteration scope for ForInStatement#1', () => {
    const {scopeManager} = parseForESLint(`
            (function () {
                let i = 20;
                for (let i in i) {
                    console.log(i);
                }
            }());
        `);

    expect(scopeManager.scopes).toHaveLength(4);

    let scope = scopeManager.scopes[0];

    expect(scope.type).toEqual('global');
    expect(scope.variables).toHaveLength(0);

    scope = scopeManager.scopes[1];
    expect(scope.type).toEqual('function');
    expect(scope.variables).toHaveLength(2);
    expect(scope.variables[0].name).toEqual('arguments');
    expect(scope.variables[1].name).toEqual('i');
    expect(scope.references).toHaveLength(1);
    expect(scope.references[0].identifier.name).toEqual('i');
    expect(scope.references[0].resolved).toEqual(scope.variables[1]);

    const iterScope = (scope = scopeManager.scopes[2]);

    expect(scope.type).toEqual('for');
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('i');
    expect(scope.references).toHaveLength(2);
    expect(scope.references[0].identifier.name).toEqual('i');
    expect(scope.references[0].resolved).toEqual(scope.variables[0]);
    expect(scope.references[1].identifier.name).toEqual('i');
    expect(scope.references[1].resolved).toEqual(scope.variables[0]);

    scope = scopeManager.scopes[3];
    expect(scope.type).toEqual('block');
    expect(scope.variables).toHaveLength(0);
    expect(scope.references).toHaveLength(2);
    expect(scope.references[0].identifier.name).toEqual('console');
    expect(scope.references[0].resolved).toEqual(null);
    expect(scope.references[1].identifier.name).toEqual('i');
    expect(scope.references[1].resolved).toEqual(iterScope.variables[0]);
  });

  it('let materialize iteration scope for ForInStatement#2', () => {
    const {scopeManager} = parseForESLint(`
            (function () {
                let i = 20;
                for (let { i, j, k } in i) {
                    console.log(i);
                }
            }());
        `);

    expect(scopeManager.scopes).toHaveLength(4);

    let scope = scopeManager.scopes[0];

    expect(scope.type).toEqual('global');
    expect(scope.variables).toHaveLength(0);

    scope = scopeManager.scopes[1];
    expect(scope.type).toEqual('function');
    expect(scope.variables).toHaveLength(2);
    expect(scope.variables[0].name).toEqual('arguments');
    expect(scope.variables[1].name).toEqual('i');
    expect(scope.references).toHaveLength(1);
    expect(scope.references[0].identifier.name).toEqual('i');
    expect(scope.references[0].resolved).toEqual(scope.variables[1]);

    const iterScope = (scope = scopeManager.scopes[2]);

    expect(scope.type).toEqual('for');
    expect(scope.variables).toHaveLength(3);
    expect(scope.variables[0].name).toEqual('i');
    expect(scope.variables[1].name).toEqual('j');
    expect(scope.variables[2].name).toEqual('k');
    expect(scope.references).toHaveLength(4);
    expect(scope.references[0].identifier.name).toEqual('i');
    expect(scope.references[0].resolved).toEqual(scope.variables[0]);
    expect(scope.references[1].identifier.name).toEqual('j');
    expect(scope.references[1].resolved).toEqual(scope.variables[1]);
    expect(scope.references[2].identifier.name).toEqual('k');
    expect(scope.references[2].resolved).toEqual(scope.variables[2]);
    expect(scope.references[3].identifier.name).toEqual('i');
    expect(scope.references[3].resolved).toEqual(scope.variables[0]);

    scope = scopeManager.scopes[3];
    expect(scope.type).toEqual('block');
    expect(scope.variables).toHaveLength(0);
    expect(scope.references).toHaveLength(2);
    expect(scope.references[0].identifier.name).toEqual('console');
    expect(scope.references[0].resolved).toEqual(null);
    expect(scope.references[1].identifier.name).toEqual('i');
    expect(scope.references[1].resolved).toEqual(iterScope.variables[0]);
  });

  it('let materialize iteration scope for ForStatement#2', () => {
    const {scopeManager} = parseForESLint(`
            (function () {
                let i = 20;
                let obj = {};
                for (let { i, j, k } = obj; i < okok; ++i) {
                    console.log(i, j, k);
                }
            }());
        `);

    expect(scopeManager.scopes).toHaveLength(4);

    let scope = scopeManager.scopes[0];

    expect(scope.type).toEqual('global');
    expect(scope.variables).toHaveLength(0);

    const functionScope = (scope = scopeManager.scopes[1]);

    expect(scope.type).toEqual('function');
    expect(scope.variables).toHaveLength(3);
    expect(scope.variables[0].name).toEqual('arguments');
    expect(scope.variables[1].name).toEqual('i');
    expect(scope.variables[2].name).toEqual('obj');
    expect(scope.references).toHaveLength(2);
    expect(scope.references[0].identifier.name).toEqual('i');
    expect(scope.references[0].resolved).toEqual(scope.variables[1]);
    expect(scope.references[1].identifier.name).toEqual('obj');
    expect(scope.references[1].resolved).toEqual(scope.variables[2]);

    const iterScope = (scope = scopeManager.scopes[2]);

    expect(scope.type).toEqual('for');
    expect(scope.variables).toHaveLength(3);
    expect(scope.variables[0].name).toEqual('i');
    expect(scope.variables[0].defs[0].type).toEqual('Variable');
    expect(scope.variables[1].name).toEqual('j');
    expect(scope.variables[1].defs[0].type).toEqual('Variable');
    expect(scope.variables[2].name).toEqual('k');
    expect(scope.variables[2].defs[0].type).toEqual('Variable');
    expect(scope.references).toHaveLength(7);
    expect(scope.references[0].identifier.name).toEqual('i');
    expect(scope.references[0].resolved).toEqual(scope.variables[0]);
    expect(scope.references[1].identifier.name).toEqual('j');
    expect(scope.references[1].resolved).toEqual(scope.variables[1]);
    expect(scope.references[2].identifier.name).toEqual('k');
    expect(scope.references[2].resolved).toEqual(scope.variables[2]);
    expect(scope.references[3].identifier.name).toEqual('obj');
    expect(scope.references[3].resolved).toEqual(functionScope.variables[2]);
    expect(scope.references[4].identifier.name).toEqual('i');
    expect(scope.references[4].resolved).toEqual(scope.variables[0]);
    expect(scope.references[5].identifier.name).toEqual('okok');
    expect(scope.references[5].resolved).toBeNull();
    expect(scope.references[6].identifier.name).toEqual('i');
    expect(scope.references[6].resolved).toEqual(scope.variables[0]);

    scope = scopeManager.scopes[3];
    expect(scope.type).toEqual('block');
    expect(scope.variables).toHaveLength(0);
    expect(scope.references).toHaveLength(4);
    expect(scope.references[0].identifier.name).toEqual('console');
    expect(scope.references[0].resolved).toBeNull();
    expect(scope.references[1].identifier.name).toEqual('i');
    expect(scope.references[1].resolved).toEqual(iterScope.variables[0]);
    expect(scope.references[2].identifier.name).toEqual('j');
    expect(scope.references[2].resolved).toEqual(iterScope.variables[1]);
    expect(scope.references[3].identifier.name).toEqual('k');
    expect(scope.references[3].resolved).toEqual(iterScope.variables[2]);
  });
});
