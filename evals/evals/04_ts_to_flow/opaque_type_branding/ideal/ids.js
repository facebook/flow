/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

// A UserId and a PostId are both strings at runtime, but the opaque
// declarations keep them distinct everywhere outside this module.
opaque type UserId: string = string;
opaque type PostId: string = string;

export function makeUserId(raw: string): UserId {
  return raw;
}

export function makePostId(raw: string): PostId {
  return raw;
}

export type {UserId, PostId};
