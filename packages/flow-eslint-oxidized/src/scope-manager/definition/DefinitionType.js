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

import keyMirror from '../keyMirror';

const DefinitionType = keyMirror({
  CatchClause: null,
  ClassName: null,
  ComponentName: null,
  NamespaceName: null,
  Enum: null,
  FunctionName: null,
  HookName: null,
  ImplicitGlobalVariable: null,
  ImportBinding: null,
  Parameter: null,
  RecordName: null,
  Type: null,
  TypeParameter: null,
  Variable: null,
});
type DefinitionTypeType = $Values<typeof DefinitionType>;

export type {DefinitionTypeType};
export {DefinitionType};
