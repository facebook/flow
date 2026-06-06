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

describe('ES6 class', () => {
  it('declaration name creates class scope', () => {
    const {scopeManager} = parseForESLint(`
            class Derived extends Base {
                constructor() {
                }
            }
            new Derived();
        `);

    expect(scopeManager.scopes).toHaveLength(3);

    let scope = scopeManager.scopes[0];

    expect(scope.type).toEqual('global');
    expect(scope.block.type).toEqual('Program');
    expect(scope.isStrict).toBe(false);
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('Derived');
    expect(scope.references).toHaveLength(1);
    expect(scope.references[0].identifier.name).toEqual('Derived');

    scope = scopeManager.scopes[1];
    expect(scope.type).toEqual('class');
    expect(scope.block.type).toEqual('ClassDeclaration');
    expect(scope.isStrict).toBe(true);
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('Derived');
    expect(scope.references).toHaveLength(1);
    expect(scope.references[0].identifier.name).toEqual('Base');

    scope = scopeManager.scopes[2];
    expect(scope.type).toEqual('function');
    expect(scope.block.type).toEqual('FunctionExpression');
    expect(scope.isStrict).toBe(true);
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('arguments');
    expect(scope.references).toHaveLength(0);
  });

  it('expression name creates class scope#1', () => {
    const {scopeManager} = parseForESLint(`
            (class Derived extends Base {
                constructor() {
                }
            });
        `);

    expect(scopeManager.scopes).toHaveLength(3);

    let scope = scopeManager.scopes[0];

    expect(scope.type).toEqual('global');
    expect(scope.block.type).toEqual('Program');
    expect(scope.isStrict).toBe(false);
    expect(scope.variables).toHaveLength(0);
    expect(scope.references).toHaveLength(0);

    scope = scopeManager.scopes[1];
    expect(scope.type).toEqual('class');
    expect(scope.block.type).toEqual('ClassExpression');
    expect(scope.isStrict).toBe(true);
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('Derived');
    expect(scope.references).toHaveLength(1);
    expect(scope.references[0].identifier.name).toBe('Base');

    scope = scopeManager.scopes[2];
    expect(scope.type).toEqual('function');
    expect(scope.block.type).toEqual('FunctionExpression');
  });

  it('expression name creates class scope#2', () => {
    const {scopeManager} = parseForESLint(`
            (class extends Base {
                constructor() {
                }
            });
        `);

    expect(scopeManager.scopes).toHaveLength(3);

    let scope = scopeManager.scopes[0];

    expect(scope.type).toEqual('global');
    expect(scope.block.type).toEqual('Program');
    expect(scope.isStrict).toBe(false);
    expect(scope.variables).toHaveLength(0);
    expect(scope.references).toHaveLength(0);

    scope = scopeManager.scopes[1];
    expect(scope.type).toEqual('class');
    expect(scope.block.type).toEqual('ClassExpression');
    expect(scope.references[0].identifier.name).toBe('Base');

    scope = scopeManager.scopes[2];
    expect(scope.type).toEqual('function');
    expect(scope.block.type).toEqual('FunctionExpression');
  });

  it('computed property key may refer variables', () => {
    const {scopeManager} = parseForESLint(`
            (function () {
                var yuyushiki = 42;
                (class {
                    [yuyushiki]() {
                    }

                    [yuyushiki + 40]() {
                    }
                });
            }());
        `);

    expect(scopeManager.scopes).toHaveLength(5);

    let scope = scopeManager.scopes[0];

    expect(scope.type).toEqual('global');
    expect(scope.block.type).toEqual('Program');
    expect(scope.isStrict).toBe(false);

    scope = scopeManager.scopes[1];
    expect(scope.type).toEqual('function');
    expect(scope.block.type).toEqual('FunctionExpression');
    expect(scope.isStrict).toBe(false);
    expect(scope.variables).toHaveLength(2);
    expect(scope.variables[0].name).toEqual('arguments');
    expect(scope.variables[1].name).toEqual('yuyushiki');
    expect(scope.references).toHaveLength(1);
    expect(scope.references[0].identifier.name).toEqual('yuyushiki');

    scope = scopeManager.scopes[2];
    expect(scope.type).toEqual('class');
    expect(scope.block.type).toEqual('ClassExpression');
    expect(scope.isStrict).toBe(true);
    expect(scope.variables).toHaveLength(0);
    expect(scope.references).toHaveLength(2);
    expect(scope.references[0].identifier.name).toEqual('yuyushiki');
    expect(scope.references[1].identifier.name).toEqual('yuyushiki');
  });

  it('regression #49', () => {
    const {scopeManager} = parseForESLint(`
            class Shoe {
                constructor() {
                    //Shoe.x = true;
                }
            }
            let shoe = new Shoe();
        `);

    expect(scopeManager.scopes).toHaveLength(3);

    const scope = scopeManager.scopes[0];

    expect(scope.type).toEqual('global');
    expect(scope.block.type).toEqual('Program');
    expect(scope.isStrict).toBe(false);
    expect(scope.variables).toHaveLength(2);
    expect(scope.variables[0].name).toEqual('Shoe');
    expect(scope.variables[1].name).toEqual('shoe');
    expect(scope.references).toHaveLength(2);
    expect(scope.references[0].identifier.name).toEqual('shoe');
    expect(scope.references[1].identifier.name).toEqual('Shoe');
  });
});
