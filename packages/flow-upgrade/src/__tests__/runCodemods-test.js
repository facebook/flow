/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import fs from 'fs-extra';
import os from 'os';
import path from 'path';

import convertLegacyUtilityTypes from '../codemods/convertLegacyUtilityTypes';
import runCodemods from '../runCodemods';

describe('runCodemods', () => {
  it('transforms each file independently', async () => {
    const dir = fs.mkdtempSync(
      path.join(os.tmpdir(), 'flow-upgrade-run-codemods-'),
    );

    const fileContents: {[string]: string} = {
      'A.js': `// @flow
// This is file A.js.
type A1 = $ReadOnlyArray<mixed>;
type A2 = $Keys<$Values<A1>>;
`,
      'B.js': `// @flow
// This is file B.js.
type B1 = $NonMaybeType<mixed>;
type B2 = $ReadOnly<B1>;
`,
    };
    const expected: {[string]: string} = {
      'A.js': `// @flow
// This is file A.js.
type A1 = ReadonlyArray<unknown>;
type A2 = keyof Values<A1>;
`,
      'B.js': `// @flow
// This is file B.js.
type B1 = NonNullable<unknown>;
type B2 = Readonly<B1>;
`,
    };

    const filePaths: Array<string> = [];
    try {
      for (const [name, contents] of Object.entries(fileContents)) {
        const filePath = path.join(dir, name);
        await fs.writeFile(filePath, contents, 'utf8');
        filePaths.push(filePath);
      }

      await runCodemods(
        [convertLegacyUtilityTypes],
        filePaths,
        {
          all: true,
          prettierOptions: {},
          silent: true,
          yes: true,
        },
      );

      for (const [name, expectedContents] of Object.entries(expected)) {
        const actual = await fs.readFile(path.join(dir, name), 'utf8');
        expect(actual).toBe(expectedContents);
      }
    } finally {
      await fs.remove(dir);
    }
  });
});
