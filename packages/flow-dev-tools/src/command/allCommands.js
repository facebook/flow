/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import typeof Base from './Base';

module.exports = ({
  'add-comments': () => require('../comment/add-commentsCommand'),
  'error-summary': () => require('../error-summary/error-summaryCommand'),
  help: () => require('../help/helpCommand'),
  'new-test': () => require('../new-test/new-testCommand'),
  'optimal-per-directory-enforcement': () =>
    require('../optimal-per-directory-enforcement/optimal-per-directory-enforcementCommand'),
  ping: () => require('../ping/pingCommand'),
  record: () => require('../record/recordCommand'),
  'remove-comments': () => require('../comment/remove-commentsCommand'),
  test: () => require('../test/testCommand'),
  'update-suppressions': () =>
    require('../update-suppressions/update-suppressionsCommand'),
  // prettier-ignore
  // @fb-only: cgroups: () => require('../facebook/cgroups/cgroupsCommand'),
  // prettier-ignore
  // @fb-only: 'cherry-pick': () => require('../facebook/release/cherry-pickCommand'),
  // prettier-ignore
  // @fb-only: 'list-commits': () => require('../facebook/release/list-commitsCommand'),
  // prettier-ignore
  // @fb-only: 'new-version': () => require('../facebook/release/new-versionCommand'),
  // prettier-ignore
  // @fb-only: 'suppression-primary-locations': () => require('../facebook/suppression-primary-locations/suppression-primary-locationsCommand'),
  // prettier-ignore
  // @fb-only: 'windtunnel-prof': () => require('../facebook/windtunnel-prof/windtunnel-profCommand'),
  // prettier-ignore
  // @fb-only: 'windtunnel-recheck-prof': () => require('../facebook/windtunnel-recheck-prof/windtunnel-recheck-profCommand'),
}: {
  [string]: () => {
    +default: interface {
      description(): string,
      go(): Promise<void>,
      showUsage(number): void,
    },
    ...
  },
});
