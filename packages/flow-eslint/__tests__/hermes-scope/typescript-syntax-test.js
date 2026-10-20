/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

'use strict';

import type {Scope} from '../../src';

import {parseForESLint, ScopeType} from '../../src';

function references(scope: Scope) {
  return scope.through.map(reference => ({
    name: reference.identifier.name,
    type: reference.isTypeReference,
    value: reference.isValueReference,
  }));
}

describe('TypeScript-compatible syntax', () => {
  test('visits satisfies annotations as types', () => {
    const {scopeManager} = parseForESLint(
      'const result = value satisfies SomeType;',
    );

    expect(references(scopeManager.scopes[1])).toEqual([
      {name: 'value', type: false, value: true},
      {name: 'SomeType', type: true, value: false},
    ]);
  });

  test('visits abstract property annotations without referencing property names', () => {
    const {scopeManager} = parseForESLint(
      'abstract class Base { abstract value: Foo; }',
    );
    const classScope = scopeManager.scopes.find(
      scope => scope.type === ScopeType.Class,
    );

    expect(classScope == null ? null : references(classScope)).toEqual([
      {name: 'Foo', type: true, value: false},
    ]);
  });

  test('visits abstract method annotations without referencing method names', () => {
    const {scopeManager} = parseForESLint(
      'abstract class Base { abstract method(x: Bar): Baz; }',
    );
    const classScope = scopeManager.scopes.find(
      scope => scope.type === ScopeType.Class,
    );

    expect(classScope == null ? null : references(classScope)).toEqual([
      {name: 'Bar', type: true, value: false},
      {name: 'Baz', type: true, value: false},
    ]);
  });

  test('visits inline type exports as types', () => {
    const {scopeManager} = parseForESLint(`
      type Foo = string;
      export {type Foo};
    `);
    const moduleScope = scopeManager.scopes[1];
    const variable = moduleScope.variables.find(({name}) => name === 'Foo');

    expect(variable?.references).toHaveLength(1);
    expect(
      variable?.references.map(reference => ({
        type: reference.isTypeReference,
        value: reference.isValueReference,
      })),
    ).toEqual([{type: true, value: false}]);
  });

  test('visits inline ambient type exports as types', () => {
    const {scopeManager} = parseForESLint(`
      type Foo = string;
      declare export {type Foo};
    `);
    const moduleScope = scopeManager.scopes[1];
    const variable = moduleScope.variables.find(({name}) => name === 'Foo');

    expect(variable?.references).toHaveLength(1);
    expect(
      variable?.references.map(reference => ({
        type: reference.isTypeReference,
        value: reference.isValueReference,
      })),
    ).toEqual([{type: true, value: false}]);
  });

  test('visits tagged template type arguments as types', () => {
    const {scopeManager} = parseForESLint('tag<Foo>`${value}`;');

    expect(references(scopeManager.scopes[1])).toEqual([
      {name: 'tag', type: false, value: true},
      {name: 'Foo', type: true, value: false},
      {name: 'value', type: false, value: true},
    ]);
  });

  test('classifies non-null expression targets as writes', () => {
    const {scopeManager} = parseForESLint(
      `
        value! = source;
        value! += source;
        value!++;
      `,
      {assertOperator: true},
    );

    expect(
      scopeManager.scopes[1].through.map(reference => ({
        name: reference.identifier.name,
        read: reference.isRead(),
        write: reference.isWrite(),
      })),
    ).toEqual([
      {name: 'value', read: false, write: true},
      {name: 'source', read: true, write: false},
      {name: 'value', read: true, write: true},
      {name: 'source', read: true, write: false},
      {name: 'value', read: true, write: true},
    ]);
  });

  test('handles parameter properties as parameters', () => {
    const {scopeManager} = parseForESLint(
      'class C { constructor(public value: Foo = init) { use(value); } }',
    );
    const functionScope = scopeManager.scopes.find(
      scope => scope.type === ScopeType.Function,
    );

    expect(functionScope?.variables.map(variable => variable.name)).toEqual([
      'arguments',
      'value',
    ]);
    expect(functionScope == null ? null : references(functionScope)).toEqual([
      {name: 'Foo', type: true, value: false},
      {name: 'init', type: false, value: true},
      {name: 'use', type: false, value: true},
    ]);
  });

  test('visits declare method annotations without referencing names', () => {
    const {scopeManager} = parseForESLint('class C { method(x: Foo): Bar; }');
    const classScope = scopeManager.scopes.find(
      scope => scope.type === ScopeType.Class,
    );

    expect(classScope == null ? null : references(classScope)).toEqual([
      {name: 'Foo', type: true, value: false},
      {name: 'Bar', type: true, value: false},
    ]);
  });
});
