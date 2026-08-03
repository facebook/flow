/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

// $FlowExpectedError[cannot-resolve-module]
import prettierConfig from '../../../.prettierrc.json';
import {unstable_translateTSDefToFlowDef} from '../../src';
import path from 'path';
import {testFixtures} from '../utils/snapshotTestUtils';

// set this to the path of the test to only run that fixture
const ONLY = '';

describe('TSDefToFlowDef', () => {
  testFixtures(
    path.resolve(__dirname, 'fixtures'),
    contents => unstable_translateTSDefToFlowDef(contents, prettierConfig),
    ONLY,
  );
});
