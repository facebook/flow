/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export opaque type Id<TEntity> = string;

export function makeId<TEntity>(raw: string): Id<TEntity> {
  return raw;
}

export function idToString<TEntity>(id: Id<TEntity>): string {
  return id;
}
