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

import type {ActivityFeedQuery as ActivityFeedQueryType} from 'ActivityFeedQuery.graphql';
import type {ActivityFeedConfig} from 'ActivityFeedConfig';
import type {PreloadedQuery} from 'RelayHooks';

import {graphql, usePreloadedQuery} from 'RelayHooks';
import * as React from 'react';

type ActivityFeedProps = Readonly<{
  queryRef: PreloadedQuery<ActivityFeedQueryType>,
  ...ActivityFeedConfig,
}>;

component ActivityFeed(
  ...{queryRef, compact, maxItems, showTimestamps}: ActivityFeedProps
) {
  const data = usePreloadedQuery(
    graphql`
      query ActivityFeedQuery($viewerId: ID!) {
        activityFeed {
          __typename
          ... on PostActivity {
            author
            preview
            visibility
          }
          ... on FriendRequestActivity {
            requester
            mutualFriendCount
            group {
              name
            }
          }
          ... on EventReminderActivity {
            eventName
            startTimeMs
          }
        }
      }
    `,
    queryRef,
  );

  const entries =
    maxItems != null ? data.activityFeed.slice(0, maxItems) : data.activityFeed;

  return (
    <ul>
      {entries.map((entry, index) => (
        <li key={index}>
          {
            match (entry) {
              {
                __typename: 'PostActivity',
                visibility: 'public' | 'friends',
                const author,
                const preview,
              } =>
                compact === true
                  ? `${author} posted`
                  : `${author} posted: ${preview}`,
              {
                __typename: 'PostActivity',
                visibility: 'private',
                const author,
                ...
              } => `${author} shared a private post`,
              {
                __typename: 'FriendRequestActivity',
                const requester,
                const mutualFriendCount,
                group: {const name},
              } =>
                `${requester} sent a friend request — ${mutualFriendCount} mutual friend${mutualFriendCount === 1 ? '' : 's'} in ${name}`,
              {
                __typename: 'FriendRequestActivity',
                const requester,
                const mutualFriendCount,
                group: null,
              } =>
                `${requester} sent a friend request — ${mutualFriendCount} mutual friend${mutualFriendCount === 1 ? '' : 's'}`,
              {
                __typename: 'EventReminderActivity',
                const eventName,
                const startTimeMs,
              } =>
                showTimestamps !== false
                  ? `${eventName} starts on ${new Date(startTimeMs).toLocaleDateString()}`
                  : eventName,
              {__typename: 'LikeActivity'} | {__typename: 'CommentActivity'} =>
                'New social activity',
              _ => 'New activity',
            }
          }
        </li>
      ))}
    </ul>
  );
}

export default ActivityFeed;
