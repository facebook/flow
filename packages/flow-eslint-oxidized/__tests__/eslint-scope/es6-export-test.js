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

describe('export declaration', () => {
  // http://people.mozilla.org/~jorendorff/es6-draft.html#sec-static-and-runtme-semantics-module-records
  it('should create variable bindings', () => {
    const {scopeManager} = parseForESLint('export var v;', {
      sourceType: 'module',
    });

    expect(scopeManager.scopes).toHaveLength(2);
    const globalScope = scopeManager.scopes[0];

    expect(globalScope.type).toEqual('global');
    expect(globalScope.variables).toHaveLength(0);
    expect(globalScope.references).toHaveLength(0);

    const scope = scopeManager.scopes[1];

    expect(scope.type).toEqual('module');
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('v');
    expect(scope.variables[0].defs[0].type).toEqual('Variable');
    expect(scope.references).toHaveLength(0);
  });

  it('should create function declaration bindings', () => {
    const {scopeManager} = parseForESLint('export default function f(){};', {
      sourceType: 'module',
    });

    expect(scopeManager.scopes).toHaveLength(3);
    const globalScope = scopeManager.scopes[0];

    expect(globalScope.type).toEqual('global');
    expect(globalScope.variables).toHaveLength(0);
    expect(globalScope.references).toHaveLength(0);

    let scope = scopeManager.scopes[1];

    expect(scope.type).toEqual('module');
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('f');
    expect(scope.variables[0].defs[0].type).toEqual('FunctionName');
    expect(scope.references).toHaveLength(0);

    scope = scopeManager.scopes[2];
    expect(scope.type).toEqual('function');
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('arguments');
    expect(scope.references).toHaveLength(0);
  });

  it('should export function expression', () => {
    const {scopeManager} = parseForESLint('export default function(){};', {
      sourceType: 'module',
    });

    expect(scopeManager.scopes).toHaveLength(3);
    const globalScope = scopeManager.scopes[0];

    expect(globalScope.type).toEqual('global');
    expect(globalScope.variables).toHaveLength(0);
    expect(globalScope.references).toHaveLength(0);

    let scope = scopeManager.scopes[1];

    expect(scope.type).toEqual('module');
    expect(scope.variables).toHaveLength(0);
    expect(scope.references).toHaveLength(0);

    scope = scopeManager.scopes[2];
    expect(scope.type).toEqual('function');
    expect(scope.variables).toHaveLength(1);
    expect(scope.variables[0].name).toEqual('arguments');
    expect(scope.references).toHaveLength(0);
  });

  it('should export literal', () => {
    const {scopeManager} = parseForESLint('export default 42;', {
      sourceType: 'module',
    });

    expect(scopeManager.scopes).toHaveLength(2);
    const globalScope = scopeManager.scopes[0];

    expect(globalScope.type).toEqual('global');
    expect(globalScope.variables).toHaveLength(0);
    expect(globalScope.references).toHaveLength(0);

    const scope = scopeManager.scopes[1];

    expect(scope.type).toEqual('module');
    expect(scope.variables).toHaveLength(0);
    expect(scope.references).toHaveLength(0);
  });

  it('should refer exported references#1', () => {
    const {scopeManager} = parseForESLint('const x = 1; export {x};', {
      sourceType: 'module',
    });

    expect(scopeManager.scopes).toHaveLength(2);
    const globalScope = scopeManager.scopes[0];

    expect(globalScope.type).toEqual('global');
    expect(globalScope.variables).toHaveLength(0);
    expect(globalScope.references).toHaveLength(0);

    const scope = scopeManager.scopes[1];

    expect(scope.type).toEqual('module');
    expect(scope.variables).toHaveLength(1);
    expect(scope.references).toHaveLength(2);
    expect(scope.references[0].identifier.name).toEqual('x');
    expect(scope.references[1].identifier.name).toEqual('x');
  });

  it('should refer exported references#2', () => {
    const {scopeManager} = parseForESLint('const v = 1; export {v as x};', {
      sourceType: 'module',
    });

    expect(scopeManager.scopes).toHaveLength(2);
    const globalScope = scopeManager.scopes[0];

    expect(globalScope.type).toEqual('global');
    expect(globalScope.variables).toHaveLength(0);
    expect(globalScope.references).toHaveLength(0);

    const scope = scopeManager.scopes[1];

    expect(scope.type).toEqual('module');
    expect(scope.variables).toHaveLength(1);
    expect(scope.references).toHaveLength(2);
    expect(scope.references[0].identifier.name).toEqual('v');
    expect(scope.references[1].identifier.name).toEqual('v');
  });

  it('should not refer exported references from other source#1', () => {
    const {scopeManager} = parseForESLint('export {x} from "mod";', {
      sourceType: 'module',
    });

    expect(scopeManager.scopes).toHaveLength(2);
    const globalScope = scopeManager.scopes[0];

    expect(globalScope.type).toEqual('global');
    expect(globalScope.variables).toHaveLength(0);
    expect(globalScope.references).toHaveLength(0);

    const scope = scopeManager.scopes[1];

    expect(scope.type).toEqual('module');
    expect(scope.variables).toHaveLength(0);
    expect(scope.references).toHaveLength(0);
  });

  it('should not refer exported references from other source#2', () => {
    const {scopeManager} = parseForESLint('export {v as x} from "mod";', {
      sourceType: 'module',
    });

    expect(scopeManager.scopes).toHaveLength(2);
    const globalScope = scopeManager.scopes[0];

    expect(globalScope.type).toEqual('global');
    expect(globalScope.variables).toHaveLength(0);
    expect(globalScope.references).toHaveLength(0);

    const scope = scopeManager.scopes[1];

    expect(scope.type).toEqual('module');
    expect(scope.variables).toHaveLength(0);
    expect(scope.references).toHaveLength(0);
  });

  it('should not refer exported references from other source#3', () => {
    const {scopeManager} = parseForESLint('export * from "mod";', {
      sourceType: 'module',
    });

    expect(scopeManager.scopes).toHaveLength(2);
    const globalScope = scopeManager.scopes[0];

    expect(globalScope.type).toEqual('global');
    expect(globalScope.variables).toHaveLength(0);
    expect(globalScope.references).toHaveLength(0);

    const scope = scopeManager.scopes[1];

    expect(scope.type).toEqual('module');
    expect(scope.variables).toHaveLength(0);
    expect(scope.references).toHaveLength(0);
  });
});
