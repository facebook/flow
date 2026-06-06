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

describe('ES6 block scope', () => {
  it('let is materialized in ES6 block scope#1', () => {
    const {scopeManager} = parseForESLint(`
            {
                let i = 20;
                i;
            }
        `);

    expect(scopeManager.scopes).toHaveLength(2); // Program and BlockStatement scope.

    let scope = scopeManager.scopes[0];

    expect(scope.type).toEqual('global');
    expect(scope.variables).toHaveLength(0); // No variable in Program scope.

    scope = scopeManager.scopes[1];
    expect(scope.type).toEqual('block');
    expect(scope.variables).toHaveLength(1); // `i` in block scope.
    expect(scope.variables[0].name).toEqual('i');
    expect(scope.references).toHaveLength(2);
    expect(scope.references[0].identifier.name).toEqual('i');
    expect(scope.references[1].identifier.name).toEqual('i');
  });

  it('function delaration is materialized in ES6 block scope', () => {
    const {scopeManager} = parseForESLint(`
            {
                function test() {
                }
                test();
            }
        `);

    expect(scopeManager.scopes).toHaveLength(3);

    let scope = scopeManager.scopes[0];

    expect(scope.type).toEqual('global');
    expect(scope.variables).toHaveLength(0);

    scope = scopeManager.scopes[1];
    expect(scope.type).toEqual('block');
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('test');
    expect(scope.references).toHaveLength(1);
    expect(scope.references[0].identifier.name).toEqual('test');

    scope = scopeManager.scopes[2];
    expect(scope.type).toEqual('function');
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('arguments');
    expect(scope.references).toHaveLength(0);
  });

  it('let is not hoistable#1', () => {
    const {scopeManager} = parseForESLint(`
            var i = 42; (1)
            {
                i;  // (2) ReferenceError at runtime.
                let i = 20;  // (2)
                i;  // (2)
            }
        `);

    expect(scopeManager.scopes).toHaveLength(2);

    const globalScope = scopeManager.scopes[0];

    expect(globalScope.type).toEqual('global');
    expect(globalScope.variables).toHaveLength(1);
    expect(globalScope.variables[0].name).toEqual('i');
    expect(globalScope.references).toHaveLength(1);

    const scope = scopeManager.scopes[1];

    expect(scope.type).toEqual('block');
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('i');
    expect(scope.references).toHaveLength(3);
    expect(scope.references[0].resolved).toEqual(scope.variables[0]);
    expect(scope.references[1].resolved).toEqual(scope.variables[0]);
    expect(scope.references[2].resolved).toEqual(scope.variables[0]);
  });

  it('let is not hoistable#2', () => {
    const {scopeManager} = parseForESLint(`
            (function () {
                var i = 42; // (1)
                i;  // (1)
                {
                    i;  // (3)
                    {
                        i;  // (2)
                        let i = 20;  // (2)
                        i;  // (2)
                    }
                    let i = 30;  // (3)
                    i;  // (3)
                }
                i;  // (1)
            }());
        `);

    expect(scopeManager.scopes).toHaveLength(4);

    const globalScope = scopeManager.scopes[0];

    expect(globalScope.type).toEqual('global');
    expect(globalScope.variables).toHaveLength(0);
    expect(globalScope.references).toHaveLength(0);

    let scope = scopeManager.scopes[1];

    expect(scope.type).toEqual('function');
    expect(scope.variables).toHaveLength(2);
    expect(scope.variables[0].name).toEqual('arguments');
    expect(scope.variables[1].name).toEqual('i');
    const v1 = scope.variables[1];

    expect(scope.references).toHaveLength(3);
    expect(scope.references[0].resolved).toEqual(v1);
    expect(scope.references[1].resolved).toEqual(v1);
    expect(scope.references[2].resolved).toEqual(v1);

    scope = scopeManager.scopes[2];
    expect(scope.type).toEqual('block');
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('i');
    const v3 = scope.variables[0];

    expect(scope.references).toHaveLength(3);
    expect(scope.references[0].resolved).toEqual(v3);
    expect(scope.references[1].resolved).toEqual(v3);
    expect(scope.references[2].resolved).toEqual(v3);

    scope = scopeManager.scopes[3];
    expect(scope.type).toEqual('block');
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('i');
    const v2 = scope.variables[0];

    expect(scope.references).toHaveLength(3);
    expect(scope.references[0].resolved).toEqual(v2);
    expect(scope.references[1].resolved).toEqual(v2);
    expect(scope.references[2].resolved).toEqual(v2);
  });
});
