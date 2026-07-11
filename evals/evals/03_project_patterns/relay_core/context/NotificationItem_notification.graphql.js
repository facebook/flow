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

import type {FragmentType} from 'relay-runtime';

declare export opaque type NotificationItem_notification$fragmentType: FragmentType;

export type NotificationItem_notification$data = Readonly<{
  title: string,
  timestamp: string,
  isRead: boolean,
  $fragmentType: NotificationItem_notification$fragmentType,
}>;

export type NotificationItem_notification$key = Readonly<{
  $data?: NotificationItem_notification$data,
  $fragmentSpreads: NotificationItem_notification$fragmentType,
  ...
}>;

// Runtime sentinel required by relay_integration so the graphql tag resolves
// to a GraphQLTaggedNode-compatible type.
export const kind: 'Fragment' = 'Fragment';
