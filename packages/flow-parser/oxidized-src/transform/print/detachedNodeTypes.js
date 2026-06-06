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
 * Minimal type-only stub for `DetachedNode<T>` and `MaybeDetachedNode<T>`.
 * Upstream hermes-transform's `src/detachedNode.js` defines these as part of
 * its broader detached-AST authoring surface used by node-builders. The
 * vendored `print()` only needs the structural types for its public API
 * signature; everything is type-erased at runtime by Babel's
 * flow-strip-types pass. By keeping a local stub we avoid pulling in the
 * full hermes-transform detached-node machinery.
 */

'use strict';

export type DetachedNode<+T> = T;
export type MaybeDetachedNode<+T> = T | DetachedNode<T>;
