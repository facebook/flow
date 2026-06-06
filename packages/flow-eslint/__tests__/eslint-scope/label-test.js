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
 Copyright (C) 2015 Yusuke Suzuki <utatane.tea@gmail.com>

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

describe('label', () => {
  it('should not create variables', () => {
    const {scopeManager} = parseForESLint(
      'function bar() { q: for(;;) { break q; } }',
    );

    expect(scopeManager.scopes).toHaveLength(3);
    const globalScope = scopeManager.scopes[0];

    expect(globalScope.type).toEqual('global');
    expect(globalScope.variables).toHaveLength(1);
    expect(globalScope.variables[0].name).toEqual('bar');
    expect(globalScope.references).toHaveLength(0);

    let scope = scopeManager.scopes[1];
    expect(scope.type).toEqual('function');
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('arguments');
    expect(scope.references).toHaveLength(0);

    scope = scopeManager.scopes[2];
    expect(scope.type).toEqual('block');
    expect(scope.variables).toHaveLength(0);
    expect(scope.references).toHaveLength(0);
  });

  it('should count child node references', () => {
    const {scopeManager} = parseForESLint(`
            var foo = 5;

            label: while (true) {
              console.log(foo);
              break;
            }
        `);

    expect(scopeManager.scopes).toHaveLength(2);
    const globalScope = scopeManager.scopes[0];

    expect(globalScope.type).toEqual('global');
    expect(globalScope.variables).toHaveLength(1);
    expect(globalScope.variables[0].name).toEqual('foo');
    expect(globalScope.through.length).toEqual(3);
    expect(globalScope.through[2].identifier.name).toEqual('foo');
    expect(globalScope.through[2].isRead()).toBe(true);

    const scope = scopeManager.scopes[1];
    expect(scope.type).toEqual('block');
    expect(scope.variables).toHaveLength(0);
    expect(scope.references).toHaveLength(2);
  });
});
