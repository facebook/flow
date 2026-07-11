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

import type {NotificationItem_notification$fragmentType} from 'NotificationItem_notification.graphql';

export type NotificationInboxQuery$variables = {userId: string};

export type NotificationInboxQuery$data = Readonly<{
  notifications: ReadonlyArray<
    Readonly<{
      id: string,
      $fragmentSpreads: NotificationItem_notification$fragmentType,
    }>,
  >,
}>;

export type NotificationInboxQuery = Readonly<{
  variables: NotificationInboxQuery$variables,
  response: NotificationInboxQuery$data,
}>;

// Runtime sentinel required by relay_integration so the graphql tag resolves
// to a GraphQLTaggedNode-compatible type.
export const kind: 'Request' = 'Request';
