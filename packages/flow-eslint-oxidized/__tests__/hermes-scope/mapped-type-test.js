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

describe('MappedType', () => {
  test('Key exists within mapped type scope', () => {
    const {scopeManager} = parseForESLint(`
      type Union = 'foo' | 'bar' | 'baz';
      type MappedType = {[key in Union]: O[key]};
    `);

    // Verify there is a module scope, variable, and reference
    expect(scopeManager.scopes).toHaveLength(3); // [global, module, Type]

    const moduleScope = scopeManager.scopes[1];
    expect(moduleScope.type).toEqual(ScopeType.Module);
    expect(moduleScope.variables).toHaveLength(2);
    expect(moduleScope.references).toHaveLength(0);

    const typeScope = scopeManager.scopes[2];
    expect(typeScope.type).toEqual(ScopeType.Type);
    expect(typeScope.variables).toHaveLength(1);
    expect(typeScope.references).toHaveLength(3);

    const variable = typeScope.variables.find(v => v.name === 'key');
    expect(variable).not.toBeNull();

    const reference = typeScope.references.find(
      r => r.identifier.name === 'key',
    );
    expect(reference).not.toBeNull();

    if (variable == null || reference == null) {
      throw new Error('Expected variable and reference to be defined');
    }

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
      name: 'key',
    });
  });
});
