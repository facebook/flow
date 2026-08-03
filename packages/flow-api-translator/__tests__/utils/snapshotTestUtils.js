/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import {ExpectedTranslationError} from '../../src/utils/ErrorUtils';
import fs from 'fs';
import glob from 'glob';
import path from 'path';

import 'jest-specific-snapshot';

export function testFixtures(
  fixturesDir: string,
  translateToString: (contents: string) => Promise<string>,
  // Set this to the path of the test to only run that fixture
  only?: string = '',
) {
  const FIXTURES = glob
    .sync(`${fixturesDir}/**/spec.{js,ts}`)
    .map(file => {
      const parsed = path.parse(file);
      const contents = fs.readFileSync(file, 'utf8');
      const fixtureName = path.relative(fixturesDir, parsed.dir);
      return {
        contents,
        name: fixtureName,
        fixturePath: file,
        translateSnapshotPath: path.join(parsed.dir, `translate-result.shot`),
      };
    })
    .filter(Boolean);

  if (FIXTURES.length === 0) {
    it.skip('no fixtures found', () => {});
  }

  for (const fixture of FIXTURES) {
    (only === fixture.name ? it.only : it)(fixture.name, async () => {
      // ensure we don't accidentally have an empty fixture
      expect(fixture.contents).not.toBe('');

      try {
        const printedResult = await translateToString(fixture.contents);
        expect(printedResult).toMatchSpecificSnapshot(
          fixture.translateSnapshotPath,
        );
      } catch (e) {
        if (e instanceof ExpectedTranslationError) {
          // errors of type TranslationError are expected, allowed and should be logged
          expect(e).toMatchSpecificSnapshot(fixture.translateSnapshotPath);
        } else {
          // unexpected errors should crash the test
          // this ensures we don't accidentally write a snapshot for a test that's unexpectedly crashing
          throw e;
        }
      }
    });
  }
}
