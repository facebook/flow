/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import {makeId, idToString, type Id} from 'Id';

type User = {name: string};
type Post = {title: string};

export function userKey(id: Id<User>): string {
  return 'user:' + idToString(id);
}

export function postKey(id: Id<Post>): string {
  return 'post:' + idToString(id);
}

export function newUserId(raw: string): Id<User> {
  return makeId<User>(raw);
}
