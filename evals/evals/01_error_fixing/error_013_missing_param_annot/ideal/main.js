/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

function formatFullName(user: {firstName: string, lastName: string}) {
  return user.firstName.trim() + ' ' + user.lastName.trim();
}

const label = formatFullName({firstName: 'Ada', lastName: 'Lovelace'});
