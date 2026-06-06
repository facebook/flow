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

describe('records', () => {
  describe('record declaration', () => {
    test('record basic', () => {
      const {scopeManager} = parseForESLint(
        `
      record R {}
      R;
      null as R;
      `,
      );

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, R]

      const scope = scopeManager.scopes[1];
      expect(scope.type).toEqual(ScopeType.Module);
      expect(scope.variables).toHaveLength(1);
      expect(scope.references).toHaveLength(2);

      const variable = scope.variables[0];
      expect(variable.name).toEqual('R');

      expect(variable.references).toHaveLength(2);
      // Value reference
      const valueReference = scope.references[0];
      expect(variable.references[0]).toBe(valueReference);
      expect(valueReference.resolved).toBe(variable);
      expect(valueReference.isValueReference).toBe(true);
      expect(valueReference.isTypeReference).toBe(false);
      // Value reference
      const typeReference = scope.references[1];
      expect(variable.references[1]).toBe(typeReference);
      expect(typeReference.resolved).toBe(variable);
      expect(typeReference.isValueReference).toBe(false);
      expect(typeReference.isTypeReference).toBe(true);

      // Verify there is one definition
      expect(variable.defs).toHaveLength(1);
      expect(variable.defs[0].type).toEqual(DefinitionType.RecordName);
      expect(variable.defs[0].node.type).toEqual('RecordDeclaration');
      expect(variable.defs[0].name).toMatchObject({
        type: 'Identifier',
        name: 'R',
      });
    });

    test('declaration name creates record scope', () => {
      const {scopeManager} = parseForESLint(
        `
      record Point {
        x: number,
        y: number,
      }
    `,
      );

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, Point]

      const moduleScope = scopeManager.scopes[1];
      expect(moduleScope.type).toEqual(ScopeType.Module);
      expect(moduleScope.variables).toHaveLength(1);
      expect(moduleScope.variables[0].name).toEqual('Point');
      expect(moduleScope.variables[0].defs[0].type).toEqual('RecordName');

      const recordScope = scopeManager.scopes[2];
      expect(recordScope.type).toEqual('record');
      expect(recordScope.block.type).toEqual('RecordDeclaration');
      expect(recordScope.isStrict).toBe(true);
    });

    test('record with type parameters', () => {
      const {scopeManager} = parseForESLint(
        `
      record Container<T> {
        value: T,
      }
    `,
      );

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, Container]

      const moduleScope = scopeManager.scopes[1];
      expect(moduleScope.type).toEqual(ScopeType.Module);
      expect(moduleScope.variables).toHaveLength(1);
      expect(moduleScope.variables[0].name).toEqual('Container');

      const recordScope = scopeManager.scopes[2];
      expect(recordScope.type).toEqual('record');
      expect(recordScope.block.type).toEqual('RecordDeclaration');
      expect(recordScope.variables).toHaveLength(1);
      expect(recordScope.variables[0].name).toEqual('T');
    });

    test('record with implements clause', () => {
      const {scopeManager} = parseForESLint(
        `
      interface IPoint {
        x: number;
        y: number;
      }
      record Point implements IPoint {
        x: number,
        y: number,
      }
    `,
      );

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, Point]

      const moduleScope = scopeManager.scopes[1];
      expect(moduleScope.type).toEqual(ScopeType.Module);
      expect(moduleScope.variables).toHaveLength(2);
      expect(moduleScope.variables[0].name).toEqual('IPoint');
      expect(moduleScope.variables[1].name).toEqual('Point');

      const recordScope = scopeManager.scopes[2]; // Point
      expect(recordScope.type).toEqual(ScopeType.Record);
      expect(recordScope.references).toHaveLength(1);
      expect(recordScope.references[0].identifier.name).toEqual('IPoint');
    });

    test('record with property default value', () => {
      const {scopeManager} = parseForESLint(
        `
      const defaultValue = 42;
      record Config {
        timeout: number = defaultValue,
      }
    `,
      );

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, Config]

      const moduleScope = scopeManager.scopes[1];
      expect(moduleScope.variables).toHaveLength(2);
      expect(moduleScope.variables[0].name).toEqual('defaultValue');
      expect(moduleScope.variables[1].name).toEqual('Config');

      const recordScope = scopeManager.scopes[2]; // Config
      expect(recordScope.type).toEqual(ScopeType.Record);
      expect(recordScope.references).toHaveLength(1);
      expect(recordScope.references[0].identifier.name).toEqual('defaultValue');
    });

    test('record with static property', () => {
      const {scopeManager} = parseForESLint(
        `
        type MyType = string;
        const myValue = 42;
        record Config {
          static DEFAULT: MyType = myValue,
        }
      `,
      );

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, Config]

      const moduleScope = scopeManager.scopes[1];
      expect(moduleScope.type).toEqual(ScopeType.Module);
      expect(moduleScope.variables).toHaveLength(3);
      expect(moduleScope.variables[0].name).toEqual('MyType');
      expect(moduleScope.variables[1].name).toEqual('myValue');
      expect(moduleScope.variables[2].name).toEqual('Config');

      const recordScope = scopeManager.scopes[2]; // Config
      expect(recordScope.type).toEqual(ScopeType.Record);
      expect(recordScope.references).toHaveLength(2);
      // Type reference to MyType
      expect(recordScope.references[0].identifier.name).toEqual('MyType');
      expect(recordScope.references[0].isTypeReference).toBe(true);
      expect(recordScope.references[0].isValueReference).toBe(false);
      // Value reference to myValue
      expect(recordScope.references[1].identifier.name).toEqual('myValue');
      expect(recordScope.references[1].isTypeReference).toBe(false);
      expect(recordScope.references[1].isValueReference).toBe(true);
    });

    test('record name is available inside its own scope', () => {
      const {scopeManager} = parseForESLint(
        `
      record Node {
        value: number,
        next: Node | null,
      }
    `,
      );

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, Node]

      const moduleScope = scopeManager.scopes[1];
      expect(moduleScope.variables).toHaveLength(1);
      expect(moduleScope.variables[0].name).toEqual('Node');

      const recordScope = scopeManager.scopes[2]; // Node
      expect(recordScope.type).toEqual(ScopeType.Record);
      expect(recordScope.references).toHaveLength(1);
      expect(recordScope.references[0].identifier.name).toBe('Node');
      expect(recordScope.references[0].resolved).toBe(moduleScope.variables[0]);
    });

    test('record definitions have correct isTypeDefinition and isVariableDefinition flags', () => {
      const {scopeManager} = parseForESLint(
        `
        record R {
          x: string,
        }
      `,
      );

      const moduleScope = scopeManager.scopes[1];
      const recordVariable = moduleScope.variables.find(v => v.name === 'R');

      expect(recordVariable).toBeDefined();
      expect(recordVariable?.defs).toHaveLength(1);
      expect(recordVariable?.defs[0].type).toEqual('RecordName');

      const recordDef = recordVariable?.defs[0];
      expect(recordDef?.isTypeDefinition).toBe(true);
      expect(recordDef?.isVariableDefinition).toBe(true);
    });
  });

  describe('record expression', () => {
    test('basic', () => {
      const {scopeManager} = parseForESLint(
        `
        record R {
          x: number,
        }
        R {x: 1};
        `,
      );

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, R]

      const moduleScope = scopeManager.scopes[1];
      expect(moduleScope.type).toEqual(ScopeType.Module);
      expect(moduleScope.variables).toHaveLength(1);
      expect(moduleScope.variables[0].name).toEqual('R');

      // Reference to R in the record expression
      expect(moduleScope.references).toHaveLength(1);
      expect(moduleScope.references[0].identifier.name).toEqual('R');
      expect(moduleScope.references[0].isValueReference).toBe(true);
      expect(moduleScope.references[0].isTypeReference).toBe(false);
    });

    test('type arguments', () => {
      const {scopeManager} = parseForESLint(
        `
        record Container<T> {
          value: T,
        }
        type MyType = string;
        Container<MyType> {value: 'hello'};
        `,
      );

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, Container]

      const moduleScope = scopeManager.scopes[1];
      expect(moduleScope.type).toEqual(ScopeType.Module);
      expect(moduleScope.variables).toHaveLength(2);
      expect(moduleScope.variables[0].name).toEqual('Container');
      expect(moduleScope.variables[1].name).toEqual('MyType');

      // Reference to Container (value) and MyType (type) in the record expression
      expect(moduleScope.references).toHaveLength(2);
      expect(moduleScope.references[0].identifier.name).toEqual('Container');
      expect(moduleScope.references[0].isValueReference).toBe(true);
      expect(moduleScope.references[0].isTypeReference).toBe(false);
      expect(moduleScope.references[1].identifier.name).toEqual('MyType');
      expect(moduleScope.references[1].isValueReference).toBe(false);
      expect(moduleScope.references[1].isTypeReference).toBe(true);
    });

    test('nested', () => {
      const {scopeManager} = parseForESLint(
        `
        record Inner {
          value: number,
        }
        record Outer {
          inner: Inner,
        }
        Outer {inner: Inner {value: 42}};
        `,
      );

      expect(scopeManager.scopes).toHaveLength(4); // [global, module, Inner, Outer]

      const moduleScope = scopeManager.scopes[1];
      expect(moduleScope.type).toEqual(ScopeType.Module);
      expect(moduleScope.variables).toHaveLength(2);
      expect(moduleScope.variables[0].name).toEqual('Inner');
      expect(moduleScope.variables[1].name).toEqual('Outer');

      // References: Outer, Inner (both value references in expressions)
      expect(moduleScope.references).toHaveLength(2);
      expect(moduleScope.references[0].identifier.name).toEqual('Outer');
      expect(moduleScope.references[0].isValueReference).toBe(true);
      expect(moduleScope.references[1].identifier.name).toEqual('Inner');
      expect(moduleScope.references[1].isValueReference).toBe(true);
    });

    test('referencing variable', () => {
      const {scopeManager} = parseForESLint(
        `
        record R {
          x: number,
        }
        const value = 42;
        R {x: value};
        `,
      );

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, R]

      const moduleScope = scopeManager.scopes[1];
      expect(moduleScope.type).toEqual(ScopeType.Module);
      expect(moduleScope.variables).toHaveLength(2);
      expect(moduleScope.variables[0].name).toEqual('R');
      expect(moduleScope.variables[1].name).toEqual('value');

      expect(moduleScope.references).toHaveLength(3);
      expect(moduleScope.references[0].identifier.name).toEqual('value');
      expect(moduleScope.references[1].identifier.name).toEqual('R');
      expect(moduleScope.references[1].isValueReference).toBe(true);
      expect(moduleScope.references[2].identifier.name).toEqual('value');
    });

    test('record expression with shorthand property', () => {
      const {scopeManager} = parseForESLint(
        `
        record R {
          x: number,
        }
        const x = 42;
        R {x};
        `,
      );

      expect(scopeManager.scopes).toHaveLength(3); // [global, module, R]

      const moduleScope = scopeManager.scopes[1];
      expect(moduleScope.type).toEqual(ScopeType.Module);
      expect(moduleScope.variables).toHaveLength(2);
      expect(moduleScope.variables[0].name).toEqual('R');
      expect(moduleScope.variables[1].name).toEqual('x');

      // References: x, R, x
      expect(moduleScope.references).toHaveLength(3);
      expect(moduleScope.references[0].identifier.name).toEqual('x');
      expect(moduleScope.references[1].identifier.name).toEqual('R');
      expect(moduleScope.references[2].identifier.name).toEqual('x');
      expect(moduleScope.references[2].isValueReference).toBe(true);
    });
  });
});
