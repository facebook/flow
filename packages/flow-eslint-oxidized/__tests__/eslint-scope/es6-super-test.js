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

describe('ES6 super', () => {
  it('is not handled as reference', () => {
    const {scopeManager} = parseForESLint(`
            class Foo extends Bar {
                constructor() {
                    super();
                }

                method() {
                    super.method();
                }
            }
        `);

    expect(scopeManager.scopes).toHaveLength(4);

    let scope = scopeManager.scopes[0];

    expect(scope.type).toEqual('global');
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('Foo');
    expect(scope.references).toHaveLength(0);

    scope = scopeManager.scopes[1];
    expect(scope.type).toEqual('class');
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('Foo');
    expect(scope.references).toHaveLength(1);

    scope = scopeManager.scopes[2];
    expect(scope.type).toEqual('function');
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('arguments');
    expect(scope.references).toHaveLength(0); // super is specially handled like `this`.

    scope = scopeManager.scopes[3];
    expect(scope.type).toEqual('function');
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('arguments');
    expect(scope.references).toHaveLength(0); // super is specially handled like `this`.
  });
});
