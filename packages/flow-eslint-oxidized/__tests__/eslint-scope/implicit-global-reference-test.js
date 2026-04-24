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
 Copyright (C) 2013 Yusuke Suzuki <utatane.tea@gmail.com>

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

describe('implicit global reference', () => {
  it('assignments global scope', () => {
    const {scopeManager} = parseForESLint(`
            var x = 20;
            x = 300;
        `);

    const scopes = scopeManager.scopes;

    expect(
      scopes.map(scope =>
        scope.variables.map(variable => variable.defs.map(def => def.type)),
      ),
    ).toEqual([[['Variable']]]);

    expect(
      scopeManager.globalScope.__implicit.variables.map(
        variable => variable.name,
      ),
    ).toEqual([]);
  });

  it('assignments global scope without definition', () => {
    const {scopeManager} = parseForESLint(`
            x = 300;
            x = 300;
        `);

    const scopes = scopeManager.scopes;

    expect(
      scopes.map(scope =>
        scope.variables.map(variable => variable.defs.map(def => def.type)),
      ),
    ).toEqual([[]]);

    expect(
      scopeManager.globalScope.__implicit.variables.map(
        variable => variable.name,
      ),
    ).toEqual(['x']);
  });

  it('assignment leaks', () => {
    const {scopeManager} = parseForESLint(`
            function outer() {
                x = 20;
            }
        `);

    const scopes = scopeManager.scopes;

    expect(
      scopes.map(scope => scope.variables.map(variable => variable.name)),
    ).toEqual([['outer'], ['arguments']]);

    expect(
      scopeManager.globalScope.__implicit.variables.map(
        variable => variable.name,
      ),
    ).toEqual(['x']);
  });

  it("assignment doesn't leak", () => {
    const {scopeManager} = parseForESLint(`
            function outer() {
                function inner() {
                    x = 20;
                }
                var x;
            }
        `);

    const scopes = scopeManager.scopes;

    expect(
      scopes.map(scope => scope.variables.map(variable => variable.name)),
    ).toEqual([['outer'], ['arguments', 'inner', 'x'], ['arguments']]);

    expect(
      scopeManager.globalScope.__implicit.variables.map(
        variable => variable.name,
      ),
    ).toEqual([]);
  });

  it('for-in-statement leaks', () => {
    const {scopeManager} = parseForESLint(`
            function outer() {
                for (x in y) { }
            }`);

    const scopes = scopeManager.scopes;

    expect(
      scopes.map(scope => scope.variables.map(variable => variable.name)),
    ).toEqual([['outer'], ['arguments'], []]);

    expect(
      scopeManager.globalScope.__implicit.variables.map(
        variable => variable.name,
      ),
    ).toEqual(['x']);
  });

  it("for-in-statement doesn't leaks", () => {
    const {scopeManager} = parseForESLint(`
            function outer() {
                function inner() {
                    for (x in y) { }
                }
                var x;
            }
        `);

    const scopes = scopeManager.scopes;

    expect(
      scopes.map(scope => scope.variables.map(variable => variable.name)),
    ).toEqual([['outer'], ['arguments', 'inner', 'x'], ['arguments'], []]);

    expect(
      scopeManager.globalScope.__implicit.variables.map(
        variable => variable.name,
      ),
    ).toEqual([]);
  });
});
