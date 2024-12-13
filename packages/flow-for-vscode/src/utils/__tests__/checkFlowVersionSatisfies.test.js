/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import satisifies from '../checkFlowVersionSatisifies';

test('matches simple versions', () => {
  expect(satisifies('0.102.0', '>= 0.100.0')).toEqual(true);
  expect(satisifies('0.102.0', '>= 0.103.0')).toEqual(false);
  expect(satisifies('0.102.0', '0.100.0 - 0.103.0')).toEqual(true);
});

test('matches pre-releases', () => {
  expect(satisifies('0.102.0-rc', '>= 0.100.0')).toEqual(true);
  expect(satisifies('0.102.0-rc.1', '>= 0.100.0')).toEqual(true);
  expect(satisifies('0.102.0-rc.1', '>= 0.103.0')).toEqual(false);
});
