/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

/*
These are a number of special-case node creation functions that we can't auto-generate.
The list of exported functions here must be kept in sync with the `NODES_WITH_SPECIAL_HANDLING`
list in `scripts/genTransformNodeTypes` to ensure there's no duplicates
*/

export * from './special-case-node-types/Comment';
export * from './special-case-node-types/DeclareExportDeclaration';
export * from './special-case-node-types/ExportNamedDeclaration';
export * from './special-case-node-types/Literal';
export * from './special-case-node-types/ObjectTypeProperty';
export * from './special-case-node-types/misc';
export * from './special-case-node-types/Property';
export * from './special-case-node-types/DeclareHook';
