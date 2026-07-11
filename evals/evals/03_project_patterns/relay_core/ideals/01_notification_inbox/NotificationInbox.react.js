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

import type {NotificationInboxQuery} from 'NotificationInboxQuery.graphql';
import type {PreloadedQuery} from 'RelayHooks';

import NotificationItem from 'NotificationItem.react';
import {graphql, usePreloadedQuery} from 'RelayHooks';

import * as React from 'react';

const notificationInboxQuery = graphql`
  query NotificationInboxQuery($userId: String!) {
    notifications(userId: $userId) {
      id
      ...NotificationItem_notification
    }
  }
`;

export default component NotificationInbox(
  queryRef: PreloadedQuery<NotificationInboxQuery>,
) {
  const data = usePreloadedQuery(notificationInboxQuery, queryRef);

  return (
    <ul>
      {data.notifications.map(notification => (
        <li key={notification.id}>
          <NotificationItem notificationRef={notification} />
        </li>
      ))}
    </ul>
  );
}
