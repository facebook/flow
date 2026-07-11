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

import type {NotificationItem_notification$key} from 'NotificationItem_notification.graphql';

import {graphql, useFragment} from 'RelayHooks';

import * as React from 'react';

const notificationFragment = graphql`
  fragment NotificationItem_notification on Notification {
    title
    timestamp
    isRead
  }
`;

export default component NotificationItem(
  notificationRef: NotificationItem_notification$key,
) {
  const notification = useFragment(notificationFragment, notificationRef);

  return (
    <div>
      <strong>{notification.title}</strong>
      <span>{notification.timestamp}</span>
      <span>{notification.isRead ? 'Read' : 'Unread'}</span>
    </div>
  );
}
