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

export type ActivityFeedQuery$variables = {viewerId: string};

export type ActivityFeedQuery$data = Readonly<{
  activityFeed: ReadonlyArray<
    | Readonly<{
        __typename: 'PostActivity',
        author: string,
        preview: string,
        visibility: 'public' | 'friends' | 'private',
      }>
    | Readonly<{
        __typename: 'FriendRequestActivity',
        requester: string,
        mutualFriendCount: number,
        group: Readonly<{name: string}> | null,
      }>
    | Readonly<{
        __typename: 'EventReminderActivity',
        eventName: string,
        startTimeMs: number,
      }>
    | Readonly<{__typename: 'LikeActivity'}>
    | Readonly<{__typename: 'CommentActivity'}>
    // Fallback for activity kinds not selected by this query.
    | Readonly<{__typename: '%other'}>,
  >,
}>;

export type ActivityFeedQuery = Readonly<{
  variables: ActivityFeedQuery$variables,
  response: ActivityFeedQuery$data,
}>;

export const kind: 'Request' = 'Request';
