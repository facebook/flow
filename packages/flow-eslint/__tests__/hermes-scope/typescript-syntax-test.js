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
});
