/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import {makeUserId, makePostId} from 'ids';
import type {UserId, PostId} from 'ids';

type Post = {
  id: PostId,
  authorId: UserId,
  title: string,
};

function createPost(id: string, authorId: string, title: string): Post {
  return {
    id: makePostId(id),
    authorId: makeUserId(authorId),
    title,
  };
}

function authorOf(post: Post): UserId {
  return post.authorId;
}

function describe(userId: UserId, post: Post): string {
  return `${userId} wrote "${post.title}" (#${post.id})`;
}

const post: Post = createPost('p1', 'u1', 'Hello, world');
const author: UserId = authorOf(post);

console.log(describe(author, post));
