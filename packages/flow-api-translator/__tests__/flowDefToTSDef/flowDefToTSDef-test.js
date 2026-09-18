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
import {translateFlowDefToTSDef, translateFlowToTSDef} from '../../src';
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

describe('flowDefToTSDef TypeScript-compatible syntax', () => {
  test('preserves mapped names', async () => {
    await expect(
      translateFlowDefToTSDef(
        'type Mapped<O> = {[K in keyof O as `get${K}`]: O[K]};',
        prettierConfig,
      ),
    ).resolves.toContain(
      'type Mapped<O> = {[K in keyof O as `get${K}`]: O[K]};',
    );
  });

  test('preserves optional tuple elements', async () => {
    await expect(
      translateFlowDefToTSDef('type Tuple = [string?];', prettierConfig),
    ).resolves.toContain('type Tuple = [string?];');
  });

  test('preserves constructor types', async () => {
    await expect(
      translateFlowDefToTSDef(
        'type Constructor = new (x: string) => Result;',
        prettierConfig,
      ),
    ).resolves.toContain('type Constructor = new (x: string) => Result;');
  });

  test('preserves abstract constructor types', async () => {
    await expect(
      translateFlowDefToTSDef(
        'type AbstractConstructor = abstract new (x: string) => Result;',
        prettierConfig,
      ),
    ).resolves.toContain(
      'type AbstractConstructor = abstract new (x: string) => Result;',
    );
  });

  test('preserves inline type exports', async () => {
    await expect(
      translateFlowDefToTSDef('declare export {type Foo};', prettierConfig),
    ).resolves.toContain('export {type Foo};');
  });
});

describe('flowToTSDef default export doc comments', () => {
  test('moves documentation to the synthetic default export', async () => {
    const result = await translateFlowToTSDef(
      `'use strict';
       /** Foo documentation */
       function Foo(): void {}
       export default Foo;`,
      prettierConfig,
      {defaultExportDocPlacement: 'export'},
    );

    expect(result.split('Foo documentation')).toHaveLength(2);
    expect(result).toContain(
      `declare function Foo(): void;
/** Foo documentation */
declare const $$EXPORT_DEFAULT_DECLARATION$$: typeof Foo;`,
    );
  });
});
