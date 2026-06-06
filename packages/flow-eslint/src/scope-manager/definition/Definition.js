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

import type {CatchClauseDefinition} from './CatchClauseDefinition';
import type {ClassNameDefinition} from './ClassNameDefinition';
import type {ComponentNameDefinition} from './ComponentNameDefinition';
import type {NamespaceNameDefinition} from './NamespaceNameDefinition';
import type {EnumDefinition} from './EnumDefinition';
import type {FunctionNameDefinition} from './FunctionNameDefinition';
import type {HookNameDefinition} from './HookNameDefinition';
import type {ImplicitGlobalVariableDefinition} from './ImplicitGlobalVariableDefinition';
import type {ImportBindingDefinition} from './ImportBindingDefinition';
import type {ParameterDefinition} from './ParameterDefinition';
import type {RecordNameDefinition} from './RecordNameDefinition';
import type {TypeDefinition} from './TypeDefinition';
import type {TypeParameterDefinition} from './TypeParameterDefinition';
import type {VariableDefinition} from './VariableDefinition';

type Definition =
  | CatchClauseDefinition
  | ClassNameDefinition
  | FunctionNameDefinition
  | ComponentNameDefinition
  | HookNameDefinition
  | ImplicitGlobalVariableDefinition
  | ImportBindingDefinition
  | NamespaceNameDefinition
  | ParameterDefinition
  | EnumDefinition
  | RecordNameDefinition
  | TypeDefinition
  | TypeParameterDefinition
  | VariableDefinition;

export type {Definition};
