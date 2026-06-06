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

import {ScopeType} from '../../src';
import {parseForESLint} from '../../src';

describe('Match', () => {
  describe('Match expression', () => {
    test('binding pattern', () => {
      const {scopeManager} = parseForESLint(`
        const e = match (x) {
          const a if (a) => a,
        };
      `);

      // [global, module, match-case-1]
      expect(scopeManager.scopes).toHaveLength(3);

      const caseScope = scopeManager.scopes[2];
      expect(caseScope.type).toEqual(ScopeType.MatchCase);
      expect(caseScope.variables).toHaveLength(1);
      expect(caseScope.references).toHaveLength(2);

      const variable = caseScope.variables[0];
      expect(variable.name).toEqual('a');
      expect(variable.references).toHaveLength(2);

      // Guard
      const guardRef = caseScope.references[0];
      expect(variable.references[0]).toBe(guardRef);
      expect(guardRef.resolved).toBe(variable);
      expect(guardRef.isValueReference).toBe(true);
      expect(guardRef.isTypeReference).toBe(false);

      // Body
      const bodyRef = caseScope.references[1];
      expect(variable.references[1]).toBe(bodyRef);
      expect(bodyRef.resolved).toBe(variable);
      expect(bodyRef.isValueReference).toBe(true);
      expect(bodyRef.isTypeReference).toBe(false);

      // Verify there is one definition
      expect(variable.defs).toHaveLength(1);
      expect(variable.defs[0].name).toMatchObject({
        type: 'Identifier',
        name: 'a',
      });
    });

    test('identifier and member patterns', () => {
      const {scopeManager} = parseForESLint(`
        let foo;
        let bar;
        const e = match (x) {
          foo => 1,
          bar.baz => 2,
        };
      `);

      // [global, module, match-case-1, match-case-2]
      expect(scopeManager.scopes).toHaveLength(4);

      // Module scope
      const moduleScope = scopeManager.scopes[1];
      expect(moduleScope.type).toEqual(ScopeType.Module);
      expect(moduleScope.variables).toHaveLength(3);
      expect(moduleScope.references).toHaveLength(2);

      const fooVar = moduleScope.variables[0];
      expect(fooVar.name).toEqual('foo');
      const barVar = moduleScope.variables[1];
      expect(barVar.name).toEqual('bar');

      // First case scope
      const caseScope1 = scopeManager.scopes[2];
      expect(caseScope1.type).toEqual(ScopeType.MatchCase);
      expect(caseScope1.variables).toHaveLength(0);
      expect(caseScope1.references).toHaveLength(1);

      const fooRef = caseScope1.references[0];
      expect(fooVar.references).toHaveLength(1);
      expect(fooVar.references[0]).toBe(fooRef);
      expect(fooRef.resolved).toBe(fooVar);
      expect(fooRef.isValueReference).toBe(true);
      expect(fooRef.isTypeReference).toBe(false);

      // Second case scope
      const caseScope2 = scopeManager.scopes[3];
      expect(caseScope2.type).toEqual(ScopeType.MatchCase);
      expect(caseScope2.variables).toHaveLength(0);
      expect(caseScope2.references).toHaveLength(1);

      const barRef = caseScope2.references[0];
      expect(barVar.references).toHaveLength(1);
      expect(barVar.references[0]).toBe(barRef);
      expect(barRef.resolved).toBe(barVar);
      expect(barRef.isValueReference).toBe(true);
      expect(barRef.isTypeReference).toBe(false);
    });

    test('wildcard pattern', () => {
      const {scopeManager} = parseForESLint(`
        const e = match (x) {
          _ => 0,
        };
      `);

      // [global, module, match-case-1]
      expect(scopeManager.scopes).toHaveLength(3);

      const caseScope = scopeManager.scopes[2];
      expect(caseScope.type).toEqual(ScopeType.MatchCase);
      expect(caseScope.variables).toHaveLength(0);
      expect(caseScope.references).toHaveLength(0);
    });

    test('object pattern', () => {
      const {scopeManager} = parseForESLint(`
        const e = match (x) {
          {foo: 1, const bar} => bar,
        };
      `);

      // [global, module, match-case-1]
      expect(scopeManager.scopes).toHaveLength(3);

      const caseScope = scopeManager.scopes[2];
      expect(caseScope.type).toEqual(ScopeType.MatchCase);
      expect(caseScope.variables).toHaveLength(1);
      expect(caseScope.references).toHaveLength(1);

      const variable = caseScope.variables[0];
      expect(variable.name).toEqual('bar');

      const reference = caseScope.references[0];
      expect(variable.references).toHaveLength(1);
      expect(variable.references[0]).toBe(reference);
      expect(reference.resolved).toBe(variable);
      expect(reference.isValueReference).toBe(true);
      expect(reference.isTypeReference).toBe(false);
    });

    test('as pattern', () => {
      const {scopeManager} = parseForESLint(`
        const e = match (x) {
          [1 as const a] => a,
          [1 as b] => b,
        };
      `);

      // [global, module, match-case-1, match-case-2]
      expect(scopeManager.scopes).toHaveLength(4);

      // First case scope
      const caseScope1 = scopeManager.scopes[2];
      expect(caseScope1.type).toEqual(ScopeType.MatchCase);
      expect(caseScope1.variables).toHaveLength(1);
      expect(caseScope1.references).toHaveLength(1);

      const aVar = caseScope1.variables[0];
      expect(aVar.name).toEqual('a');

      const aRef = caseScope1.references[0];
      expect(aVar.references).toHaveLength(1);
      expect(aVar.references[0]).toBe(aRef);
      expect(aRef.resolved).toBe(aVar);
      expect(aRef.isValueReference).toBe(true);
      expect(aRef.isTypeReference).toBe(false);

      // Second case scope
      const caseScope2 = scopeManager.scopes[3];
      expect(caseScope2.type).toEqual(ScopeType.MatchCase);
      expect(caseScope2.variables).toHaveLength(1);
      expect(caseScope2.references).toHaveLength(1);

      const bVar = caseScope2.variables[0];
      expect(bVar.name).toEqual('b');

      const bRef = caseScope2.references[0];
      expect(bVar.references).toHaveLength(1);
      expect(bVar.references[0]).toBe(bRef);
      expect(bRef.resolved).toBe(bVar);
      expect(bRef.isValueReference).toBe(true);
      expect(bRef.isTypeReference).toBe(false);
    });

    test('or pattern', () => {
      const {scopeManager} = parseForESLint(`
        const e = match (x) {
          [const a] | {foo: const a} => a,
        };
      `);

      // [global, module, match-case-1]
      expect(scopeManager.scopes).toHaveLength(3);

      const caseScope = scopeManager.scopes[2];
      expect(caseScope.type).toEqual(ScopeType.MatchCase);
      expect(caseScope.variables).toHaveLength(1);
      expect(caseScope.references).toHaveLength(1);

      const variable = caseScope.variables[0];
      expect(variable.name).toEqual('a');
      expect(variable.references).toHaveLength(1);

      const reference = caseScope.references[0];
      expect(variable.references[0]).toBe(reference);
      expect(reference.resolved).toBe(variable);
      expect(reference.isValueReference).toBe(true);
      expect(reference.isTypeReference).toBe(false);

      // Two definitions
      expect(variable.defs).toHaveLength(2);
      expect(variable.defs[0].name).toMatchObject({
        type: 'Identifier',
        name: 'a',
      });
      expect(variable.defs[1].name).toMatchObject({
        type: 'Identifier',
        name: 'a',
      });
    });
  });

  describe('Match statement', () => {
    test('basic', () => {
      const {scopeManager} = parseForESLint(`
        match (x) {
          const a => {
            a
          },
        };
      `);

      // [global, module, match-case-1, match-case-1-body]
      expect(scopeManager.scopes).toHaveLength(4);

      const caseScope = scopeManager.scopes[2];
      expect(caseScope.type).toEqual(ScopeType.MatchCase);
      expect(caseScope.variables).toHaveLength(1);
      expect(caseScope.references).toHaveLength(0);

      const caseBodyScope = scopeManager.scopes[3];
      expect(caseBodyScope.variables).toHaveLength(0);
      expect(caseBodyScope.references).toHaveLength(1);

      const variable = caseScope.variables[0];
      expect(variable.name).toEqual('a');

      const reference = caseBodyScope.references[0];
      expect(variable.references).toHaveLength(1);
      expect(variable.references[0]).toBe(reference);
      expect(reference.resolved).toBe(variable);
      expect(reference.isValueReference).toBe(true);
      expect(reference.isTypeReference).toBe(false);

      expect(variable.defs).toHaveLength(1);
      expect(variable.defs[0].name).toMatchObject({
        type: 'Identifier',
        name: 'a',
      });
    });
  });
});
