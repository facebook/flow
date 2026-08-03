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
import {translateFlowDefToTSDef} from '../../src';
import path from 'path';
import {testFixtures} from '../utils/snapshotTestUtils';

// set this to the path of the test to only run that fixture
const ONLY = '';

describe('flowDefToTSDef', () => {
  testFixtures(
    path.resolve(__dirname, 'fixtures'),
    contents => translateFlowDefToTSDef(contents, prettierConfig),
    ONLY,
  );
});

describe('flowDefToTSDef optional members include undefined', () => {
  // Flow optional members permit `undefined`, so the faithful translation adds
  // an explicit `| undefined`.
  test('adds `| undefined` to optional members', async () => {
    const source = [
      'export type Foo = {',
      '  plain?: number,',
      '  union?: number | string,',
      '  nullable?: ?number,',
      '  fn?: () => void,',
      '  required: number,',
      '};',
      '',
    ].join('\n');
    const result = await translateFlowDefToTSDef(source, prettierConfig);

    expect(result).toContain('plain?: number | undefined;');
    expect(result).toContain('union?: number | string | undefined;');
    expect(result).toContain('fn?: (() => void) | undefined;');
    // Already-nullable members keep their existing `undefined` (not doubled).
    expect(result).toContain('nullable?: null | undefined | number;');
    expect(result).not.toContain('| undefined | undefined');
    // Required members are untouched.
    expect(result).toContain('required: number;');
  });
});
