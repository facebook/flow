/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

'use strict';

import type {Program} from 'flow-estree-oxidized';
import type {Scope} from './Scope';
import type {ScopeManager} from '../ScopeManager';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';
import {DefinitionType} from '../definition/DefinitionType';

class ModuleScope extends ScopeBase<typeof ScopeType.Module, Program, Scope> {
  declare +type: typeof ScopeType.Module;

  constructor(
    scopeManager: ScopeManager,
    upperScope: ModuleScope['upper'],
    block: ModuleScope['block'],
  ) {
    super(scopeManager, ScopeType.Module, upperScope, block, false);
  }

  close(scopeManager: ScopeManager): Scope | null {
    const result = super.close(scopeManager);

    // handle this case:
    /*
    declare function foo(): void;
    declare function foo(arg: string): string;
    export function foo(arg?: string) {
      return arg;
    }
    */

    for (const variable of this.variables) {
      if (variable.defs.length <= 1) {
        continue;
      }

      for (const def of variable.defs) {
        if (def.type !== DefinitionType.FunctionName) {
          break;
        }

        if (
          def.node.type === 'FunctionDeclaration' &&
          (def.node.parent.type === 'ExportNamedDeclaration' ||
            def.node.parent.type === 'ExportDefaultDeclaration')
        ) {
          variable.eslintUsed = true;
          break;
        }
      }
    }

    return result;
  }
}

export {ModuleScope};
