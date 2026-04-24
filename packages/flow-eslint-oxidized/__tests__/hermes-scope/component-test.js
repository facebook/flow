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

import {DefinitionType, ScopeType} from '../../src';
import {parseForESLint} from '../../src';

describe('Component', () => {
  describe('Declaration', () => {
    test('declaration name', () => {
      const {scopeManager} = parseForESLint(`
        component E() {}
        E;
      `);

      // Verify there is a module scope, variable, and reference
      expect(scopeManager.scopes).toHaveLength(3); // [global, module, E]

      const scope = scopeManager.scopes[1];
      expect(scope.type).toEqual(ScopeType.Module);
      expect(scope.variables).toHaveLength(1);
      expect(scope.references).toHaveLength(1);

      const variable = scope.variables[0];
      const reference = scope.references[0];
      expect(variable.name).toEqual('E');

      // Verify that reference is resolved
      expect(variable.references).toHaveLength(1);
      expect(variable.references[0]).toBe(reference);
      expect(reference.resolved).toBe(variable);
      expect(reference.isValueReference).toBe(true);
      expect(reference.isTypeReference).toBe(false);

      // Verify there is one definition
      expect(variable.defs).toHaveLength(1);
      expect(variable.defs[0].type).toEqual(DefinitionType.ComponentName);
      expect(variable.defs[0].node.type).toEqual('ComponentDeclaration');
      expect(variable.defs[0].name).toMatchObject({
        type: 'Identifier',
        name: 'E',
      });
    });

    it('parameters should be resolved', () => {
      const {scopeManager} = parseForESLint(`
        component Foo(b) {
          return b;
        }
      `);

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, Foo]

      const scope = scopeManager.scopes[2];

      expect(scope.variables).toHaveLength(1); // [b]
      expect(scope.references).toHaveLength(1); // [b]

      const variable = scope.variables[0];
      const reference = scope.references[0];

      // Verify that reference is resolved
      expect(variable.references).toHaveLength(1);
      expect(variable.references[0]).toBe(reference);
      expect(reference.resolved).toBe(variable);
      expect(reference.isValueReference).toBe(true);
      expect(reference.isTypeReference).toBe(false);

      // Verify there is one definition
      expect(variable.defs).toHaveLength(1);
      expect(variable.defs[0].type).toEqual(DefinitionType.Parameter);
      expect(variable.defs[0].node.type).toEqual('ComponentDeclaration');
      expect(variable.defs[0].name).toMatchObject({
        type: 'Identifier',
        name: 'b',
      });
      expect(variable.defs[0].name.parent.type).toEqual('ComponentParameter');
    });

    it('default parameters should be resolved', () => {
      const {scopeManager} = parseForESLint(`
        let a = 0;
        component Foo(b = a) {}
      `);

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, Foo]

      const scope = scopeManager.scopes[2];

      expect(scope.variables).toHaveLength(1); // [b]
      expect(scope.references).toHaveLength(2); // [b, a]

      const reference = scope.references[1];

      expect(reference.from).toEqual(scope);
      expect(reference.identifier.name).toEqual('a');
      expect(reference.resolved).toEqual(scopeManager.scopes[1].variables[0]);
      expect(reference.writeExpr).toBeUndefined();
      expect(reference.isWrite()).toBe(false);
      expect(reference.isRead()).toBe(true);
    });

    it('type parameters should be resolved', () => {
      const {scopeManager} = parseForESLint(`
        component Foo<T>() renders T {}
      `);

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, Foo]

      const scope = scopeManager.scopes[2];

      expect(scope.variables).toHaveLength(1); // [T]
      expect(scope.references).toHaveLength(1); // [T]

      const variable = scope.variables[0];
      const reference = scope.references[0];

      // Verify that reference is resolved
      expect(variable.references).toHaveLength(1);
      expect(variable.references[0]).toBe(reference);
      expect(reference.resolved).toBe(variable);
      expect(reference.isValueReference).toBe(false);
      expect(reference.isTypeReference).toBe(true);

      // Verify there is one definition
      expect(variable.defs).toHaveLength(1);
      expect(variable.defs[0].type).toEqual(DefinitionType.TypeParameter);
      expect(variable.defs[0].node.type).toEqual('TypeParameter');
      expect(variable.defs[0].name).toMatchObject({
        type: 'Identifier',
        name: 'T',
      });
    });
  });
  describe('Declare Declaration', () => {
    test('declaration name', () => {
      const {scopeManager} = parseForESLint(`
        declare component E();
        E;
      `);

      // Verify there is a module scope, variable, and reference
      expect(scopeManager.scopes).toHaveLength(2); // [global, module]

      const scope = scopeManager.scopes[1];
      expect(scope.type).toEqual(ScopeType.Module);
      expect(scope.variables).toHaveLength(1);
      expect(scope.references).toHaveLength(1);

      const variable = scope.variables[0];
      const reference = scope.references[0];
      expect(variable.name).toEqual('E');

      // Verify that reference is resolved
      expect(variable.references).toHaveLength(1);
      expect(variable.references[0]).toBe(reference);
      expect(reference.resolved).toBe(variable);
      expect(reference.isValueReference).toBe(true);
      expect(reference.isTypeReference).toBe(false);

      // Verify there is one definition
      expect(variable.defs).toHaveLength(1);
      expect(variable.defs[0].type).toEqual(DefinitionType.ComponentName);
      expect(variable.defs[0].node.type).toEqual('DeclareComponent');
      expect(variable.defs[0].name).toMatchObject({
        type: 'Identifier',
        name: 'E',
      });
    });

    it('parameters should not be resolved', () => {
      const {scopeManager} = parseForESLint(`
        declare component Foo(b: B);
      `);

      expect(scopeManager.scopes).toHaveLength(2); // [global, module]

      const scope = scopeManager.scopes[1];

      expect(scope.variables).toHaveLength(1); // [Foo]
      expect(scope.references).toHaveLength(1); // [B]
    });

    it('type parameters should be resolved', () => {
      const {scopeManager} = parseForESLint(`
        declare component Foo<T>() renders T;
      `);

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, Foo]

      const scope = scopeManager.scopes[2];

      expect(scope.variables).toHaveLength(1); // [T]
      expect(scope.references).toHaveLength(1); // [T]

      const variable = scope.variables[0];
      const reference = scope.references[0];

      // Verify that reference is resolved
      expect(variable.references).toHaveLength(1);
      expect(variable.references[0]).toBe(reference);
      expect(reference.resolved).toBe(variable);
      expect(reference.isValueReference).toBe(false);
      expect(reference.isTypeReference).toBe(true);

      // Verify there is one definition
      expect(variable.defs).toHaveLength(1);
      expect(variable.defs[0].type).toEqual(DefinitionType.TypeParameter);
      expect(variable.defs[0].node.type).toEqual('TypeParameter');
      expect(variable.defs[0].name).toMatchObject({
        type: 'Identifier',
        name: 'T',
      });
    });
  });
  describe('Type', () => {
    it('parameters should not be resolved', () => {
      const {scopeManager} = parseForESLint(`
        type Foo = component(b: B);
      `);

      expect(scopeManager.scopes).toHaveLength(2); // [global, module]

      const scope = scopeManager.scopes[1];

      expect(scope.variables).toHaveLength(1); // [Foo]
      expect(scope.references).toHaveLength(1); // [B]
    });

    it('type parameters should be resolved', () => {
      const {scopeManager} = parseForESLint(`
        type Foo = component<T>() renders T;
      `);

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, Foo]

      const scope = scopeManager.scopes[2];

      expect(scope.variables).toHaveLength(1); // [T]
      expect(scope.references).toHaveLength(1); // [T]

      const variable = scope.variables[0];
      const reference = scope.references[0];

      // Verify that reference is resolved
      expect(variable.references).toHaveLength(1);
      expect(variable.references[0]).toBe(reference);
      expect(reference.resolved).toBe(variable);
      expect(reference.isValueReference).toBe(false);
      expect(reference.isTypeReference).toBe(true);

      // Verify there is one definition
      expect(variable.defs).toHaveLength(1);
      expect(variable.defs[0].type).toEqual(DefinitionType.TypeParameter);
      expect(variable.defs[0].node.type).toEqual('TypeParameter');
      expect(variable.defs[0].name).toMatchObject({
        type: 'Identifier',
        name: 'T',
      });
    });
  });
});
